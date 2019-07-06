{-|
Module      : System.Linux.RTNetlink.Packet
Description : Low-level typeclasses, functions and ADTs for making netlink
              packets.
Copyright   : (c) Formaltech Inc. 2017
License     : BSD3
Maintainer  : protob3n@gmail.com
Stability   : experimental
Portability : Linux
-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
module System.Linux.RTNetlink.Packet (
    -- * Low-level headers
      NLMsgHdr(..)
    , nlMsgHdrIsError
    , splitMessages
    -- * Attributes
    , Attribute(..)
    , AttributeList(..)
    , AttributeType
    , attributeType
    , attributeData
    , findAttribute
    , findAttributeData
    , findAttributeDecode
    , findAttributeGet
    , findAttributeCString
    , cStringAttr
    , word32Attr
    , word32AttrPart
    , word16Attr
    , word16AttrPart
    -- * Sized data
    , Sized(..)
    , putAligned
    -- * Monoidal bit flags
    , ChangeFlags(..)
    , applyChangeFlags
    , applyChangeFlags'
    , setChangeFlags
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Bits (Bits((.|.), (.&.), complement, xor, zeroBits), FiniteBits)
import Data.Bits.ByteString ()
import Data.List (unfoldr, find, sort)
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid, mempty, mappend)
import Data.Semigroup (Semigroup(..))
import Data.Serialize
import Data.Word (Word16,Word32)
import qualified Data.ByteString as S

import System.Linux.RTNetlink.Util

#include <linux/netlink.h>

-- | ADT corresponding to @struct nlmsghdr@ from @linux/netlink.h@.
data NLMsgHdr = NLMsgHdr
    { nlMsgLength :: Word32 -- ^ Total message length (@nlmsg_len@).
    , nlMsgType   :: Word16 -- ^ Message type (@nlmsg_type@).
    , nlMsgFlags  :: Word16 -- ^ Top-level flags (@nlmsg_flags@).
    , nlMsgSeqNum :: Word32 -- ^ Sequence number (@nlmsg_seq@).
    , nlMsgPid    :: Word32 -- ^ Destination address (@nlmsg_pid@).
    } deriving (Show, Eq)
instance Sized NLMsgHdr where
    size = const #{const sizeof(struct nlmsghdr)}
instance Serialize NLMsgHdr where
    put NLMsgHdr {..} = do
        putWord32host nlMsgLength
        putWord16host nlMsgType
        putWord16host nlMsgFlags
        putWord32host nlMsgSeqNum
        putWord32host nlMsgPid
    get = NLMsgHdr
        <$> getWord32host
        <*> getWord16host
        <*> getWord16host
        <*> getWord32host
        <*> getWord32host

-- | Get the type of a message started by an 'NLMsgHdr'.
nlMsgHdrType :: S.ByteString -> Word16
nlMsgHdrType = either (const 0) nlMsgType . decode

-- | Return @True@ iff the message is an error, assuming the provided
-- 'S.ByteString' is headed by an 'NLMsgHdr'.
nlMsgHdrIsError :: S.ByteString -> Bool
nlMsgHdrIsError = (== #{const NLMSG_ERROR}) . nlMsgHdrType

-- | Get the size of a message started by an 'NLMsgHdr'.
nlMsgHdrSize :: Integral n => S.ByteString -> n
nlMsgHdrSize = either (const 0) (fromIntegral . nlMsgLength) . decode

-- | Split a ByteString into multiple messages using their 'NLMsgHdr's.
splitMessages :: S.ByteString -> [S.ByteString]
splitMessages = unfoldr $ \bs -> do
    let sz = nlMsgHdrSize bs
    guard $ sz > 0 && sz <= S.length bs
    return . S.splitAt sz $ bs

-- | Typeclass for data with a defined size. This lets us get sizes to use for
-- constructing headers.
class Sized s where
    -- | Size of data.
    size :: Integral i => s -> i
    -- | Size of data with alignment padding added.
    sizeAligned :: Integral a => a -> s -> a
    sizeAligned a s = ((size s + (a-1)) `div` a) * a
    {-# MINIMAL size #-}
instance Sized () where
    size = const 0
instance Sized S.ByteString where
    size = fromIntegral . S.length

-- | Pad a 'S.ByteString' to a given alignment.
putAligned :: Integral a => a -> Putter S.ByteString
putAligned a bs = do
    putByteString $ bs
    putByteString $ S.replicate (fromIntegral $ sizeAligned a bs - size bs) 0

-- | Type identifier for an 'Attribute'.
type AttributeType = Word16

-- | ADT representing a possibly nested netlink attribute.
data Attribute
    = Attribute     AttributeType S.ByteString
    -- ^ Simple attribute.
    | AttributeNest AttributeType [Attribute]
    -- ^ Nested attribute.
    | AttributePart AttributeType S.ByteString S.ByteString
    -- ^ Composable attribute.
    deriving (Show, Eq)
instance Ord Attribute where
    Attribute     n _   `compare` Attribute     m _   = n `compare` m
    AttributeNest n _   `compare` AttributeNest m _   = n `compare` m
    AttributePart n _ _ `compare` AttributePart m _ _ = n `compare` m
    Attribute     _ _   `compare` _                   = GT
    _                   `compare` Attribute     _ _   = LT
    AttributeNest _ _   `compare` _                   = LT
    _                   `compare` AttributeNest _ _   = GT
instance Sized Attribute where
    size (Attribute     _ bs)   = #{const sizeof(struct nlattr)} + size bs
    size (AttributeNest _ as)   = #{const sizeof(struct nlattr)} + size (AttributeList as)
    size (AttributePart _ bs m) = #{const sizeof(struct nlattr)} + min (size bs) (size m)
instance Serialize Attribute where
    put a = do
        putWord16host $ size a
        case a of
            Attribute t bs -> do
                putWord16host t
                putAligned (4::Integer) bs
            AttributeNest t as -> do
                putWord16host $ t .|. #{const NLA_F_NESTED}
                put           $ AttributeList as
            AttributePart t bs mask -> do
                putWord16host t
                putAligned (4::Integer) $ bs .&. mask
    get = do
        nla_len  <- fromIntegral <$> getWord16host
        nla_type <- getWord16host
        nla_data <- getByteString $ nla_len - #{const sizeof(struct nlattr)}
        skip $ sizeAligned 4 nla_data - size nla_data
        -- Note: The kernel does not presently seem to set NLA_F_NESTED, so
        -- this doesn't work. Instead, we need to handle getting nested
        -- attributes manually.
        if nla_type .&. #{const NLA_F_NESTED} == 0
            then return $ Attribute nla_type nla_data
            else do
                AttributeList as <- get
                return $ AttributeNest (nla_type `xor` #{const NLA_F_NESTED}) as

-- | A collection of netlink attributes.
newtype AttributeList = AttributeList [Attribute]
    deriving (Show, Eq)
instance Semigroup AttributeList where
    AttributeList l1 <> AttributeList l2 = AttributeList $ l1 <> l2
instance Monoid AttributeList where
    mempty = AttributeList []
    mappend = (<>)
instance Sized AttributeList where
    size l = let AttributeList as = combineAttrs l in sum $ fmap (sizeAligned 4) as
instance Serialize AttributeList where
    put l = let AttributeList as = combineAttrs l in mapM_ put as
    get = AttributeList <$> unfoldM getMaybeAttribute
        where
        getMaybeAttribute = runMaybeT $ do
            r <- lift $ fmap fromIntegral remaining
            guard $ r >= #{const sizeof(struct nlattr)}
            l <- lift $ lookAhead getWord16host
            guard $ l >= #{const sizeof(struct nlattr)} && r >= l
            lift get

combineAttrs (AttributeList as) = AttributeList $ go as
    where
    go = foldr combine mempty . sort
    combine (AttributeNest m l1) (AttributeNest n l2 : as)
        | m == n = (AttributeNest n . go $ l1 <> l2) : as
    combine (AttributePart m bs1 m1) (AttributePart n bs2 m2 : as)
        | m == n = (AttributePart n ((bs1.&.m1) .|. (bs2.&.m2)) (m1.|.m2)) : as
    combine a as = a : as

-- | Construct an 'Attribute' with a null-byte-terminated string as data.
cStringAttr :: AttributeType -> S.ByteString -> Attribute
cStringAttr t bs = Attribute t $ bs `S.snoc` 0

-- | Construct an 'Attribute' with a 32-bit word in host byte-order as data.
word32Attr :: AttributeType -> Word32 -> Attribute
word32Attr t = Attribute t . runPut . putWord32host

-- | Composable 'Attribute' with a 32-bit word in host byte-order as data. The
-- second 'Word32' argument is a mask of bits we care about so that this
-- attribute can be combined with others of the same type.
word32AttrPart :: AttributeType -> Word32 -> Word32 -> Attribute
word32AttrPart t bits mask = AttributePart t (put' bits) (put' mask)
    where put' = runPut . putWord32host

-- | Construct an 'Attribute' with a 16-bit word in host byte-order as data.
word16Attr :: AttributeType -> Word16 -> Attribute
word16Attr t = Attribute t . runPut . putWord16host

-- | Composable 'Attribute' with a 16-bit word in host byte-order as data. The
-- second 'Word16' argument is a mask of bits we care about so that this
-- attribute can be combined with others of the same type.
word16AttrPart :: AttributeType -> Word16 -> Word16 -> Attribute
word16AttrPart t bits mask = AttributePart t (put' bits) (put' mask)
    where put' = runPut . putWord16host

-- | Get the type of an 'Attribute'.
attributeType :: Attribute -> AttributeType
attributeType (Attribute     t _)   = t
attributeType (AttributeNest t _)   = t
attributeType (AttributePart t _ _) = t

-- | Get the data from a simple 'Attribute'.
attributeData :: Attribute -> Maybe S.ByteString
attributeData (Attribute     _ bs)   = Just bs
attributeData (AttributeNest _ _)    = Nothing
attributeData (AttributePart _ bs m) = Just $ bs .&. m

-- | Search for an 'Attribute' in a possibly nested list using the
-- 'AttributeType' to look for at each level. Unfortunately, the kernel does
-- not presently seem to set NLA_F_NESTED on nested attribute types. Until
-- this is changed in the kernel, we need to traverse nested elements manually.
findAttribute :: [AttributeType] -> AttributeList -> Maybe Attribute
findAttribute ts (AttributeList as) = do
    t <- listToMaybe ts
    a <- find ((==t) . attributeType) as
    case tail ts of
        []  -> return a
        ts' -> case a of
            AttributeNest _ as' -> findAttribute ts' (AttributeList as')
            _                   -> Nothing

-- | Search for an 'Attribute' and return its data field.
findAttributeData :: [AttributeType] -> AttributeList -> Maybe S.ByteString
findAttributeData ts l = attributeData =<< findAttribute ts l

-- | Search for an 'Attribute'; decode and return its data field.
findAttributeDecode :: Serialize a => [AttributeType] -> AttributeList -> Maybe a
findAttributeDecode ts l = decodeMaybe =<< attributeData =<< findAttribute ts l

-- | Search for an 'Attribute' and return its data field, minus any null bytes.
findAttributeCString :: [AttributeType] -> AttributeList -> Maybe S.ByteString
findAttributeCString ts l = S.takeWhile (/=0) <$> findAttributeData ts l

-- | Search for an 'Attribute', run a getter on it, and return the result.
findAttributeGet :: Get a -> [AttributeType] -> AttributeList -> Maybe a
findAttributeGet g ts l = runGetMaybe g =<< attributeData =<< findAttribute ts l

-- | A flags bitfield encoded as a set of changes to an initial value, which can
-- can be combined using the 'Monoid' instance. This 'Monoid' instance is *not*
-- commutative. Flags set or cleared on the right will override those on the
-- left.
data ChangeFlags a = ChangeFlags
    { cfFlags :: a -- ^ Flag bits
    , cfMask  :: a -- ^ Mask of flag bits to use. Other bits will be ignored.
    } deriving Show
instance Bits a => Eq (ChangeFlags a) where
    ChangeFlags f m == ChangeFlags g n = m == n && (f .&. m) == (g .&. n)
instance (Bits a, FiniteBits a) => Semigroup (ChangeFlags a) where
    f <> g = ChangeFlags
        { cfFlags = applyChangeFlags g $ applyChangeFlags f zeroBits
        , cfMask  = cfMask f .|. cfMask g
        }
instance (Bits a, FiniteBits a) => Monoid (ChangeFlags a) where
    mempty = ChangeFlags zeroBits zeroBits
    mappend = (<>)

-- | Apply a change to an existing flags bitfield.
applyChangeFlags :: Bits a => ChangeFlags a -> a -> a
applyChangeFlags ChangeFlags {..} b = (cfFlags .&. cfMask) `xor` (b .&. complement cfMask)

-- | Apply a change to the all-zeroes bit field.
applyChangeFlags' :: Bits a => ChangeFlags a -> a
applyChangeFlags' f = applyChangeFlags f zeroBits

-- | Set 'cfFlags' and 'cfMask' to the same value.
setChangeFlags :: Bits a => a -> ChangeFlags a
setChangeFlags a = ChangeFlags a a
