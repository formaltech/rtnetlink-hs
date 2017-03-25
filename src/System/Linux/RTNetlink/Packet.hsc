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
    , cStringAttr
    , word32Attr
    , word16Attr
    -- * Sized data
    , Sized(..)
    , putAligned
    ) where

import Control.Monad (guard)
import Control.Monad.Loops (unfoldM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Bits ((.|.), (.&.), xor)
import Data.List (unfoldr, find)
import Data.Serialize
import Data.Word (Word16,Word32)
import Data.Maybe (listToMaybe)
import qualified Data.ByteString as S

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
    size        :: Integral i => s -> i
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
    = Attribute     AttributeType S.ByteString -- ^ Simple attribute.
    | AttributeNest AttributeType [Attribute]  -- ^ Nested attribute.
    deriving (Show, Eq)
instance Sized Attribute where
    size (Attribute     _ bs) = #{const sizeof(struct nlattr)} + size bs
    size (AttributeNest _ as) = #{const sizeof(struct nlattr)} + size (AttributeList as)
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
    get = do
        nla_len  <- fromIntegral <$> getWord16host
        nla_type <- getWord16host
        nla_data <- getByteString $ nla_len - #{const sizeof(struct nlattr)}
        skip $ sizeAligned 4 nla_data - size nla_data
        if nla_type .&. #{const NLA_F_NESTED} == 0
            then return $ Attribute nla_type nla_data
            else do
                AttributeList as <- get
                return $ AttributeNest (nla_type `xor` #{const NLA_F_NESTED}) as

-- | A collection of netlink attributes.
newtype AttributeList = AttributeList [Attribute]
    deriving (Show, Eq)
instance Sized AttributeList where
    size (AttributeList as) = sum $ fmap (sizeAligned 4) as
instance Serialize AttributeList where
    put (AttributeList as) = mapM_ put as
    get = AttributeList <$> unfoldM getMaybeAttribute
        where
        getMaybeAttribute = runMaybeT $ do
            r <- lift $ fmap fromIntegral remaining
            guard $ r >= #{const sizeof(struct nlattr)}
            l <- lift $ lookAhead getWord16host
            guard $ l >= #{const sizeof(struct nlattr)} && r >= l
            lift get
instance Monoid AttributeList where
    mempty = AttributeList []
    AttributeList a `mappend` AttributeList b = AttributeList $ a ++ b

-- | Construct an 'Attribute' with a null-byte-terminated string as data.
cStringAttr :: AttributeType -> S.ByteString -> Attribute
cStringAttr t bs = Attribute t $ bs `S.snoc` 0

-- | Construct an 'Attribute' with a 32-bit word as data.
word32Attr :: AttributeType -> Word32 -> Attribute
word32Attr t = Attribute t . runPut . putWord32host

-- | Construct an 'Attribute' with a 16-bit word as data.
word16Attr :: AttributeType -> Word16 -> Attribute
word16Attr t = Attribute t . runPut . putWord16host

-- | Get the type of an 'Attribute'.
attributeType :: Attribute -> AttributeType
attributeType (Attribute     t _) = t
attributeType (AttributeNest t _) = t

-- | Get the data from a simple 'Attribute'.
attributeData :: Attribute -> Maybe S.ByteString
attributeData (Attribute     _ bs) = Just bs
attributeData (AttributeNest _ _)  = Nothing

-- | Search for an 'Attribute' in a possibly nested list using the
-- 'AttributeType' to look for at each level.
findAttribute :: [AttributeType] -> AttributeList -> Maybe Attribute
findAttribute ts (AttributeList as) = do
    t <- listToMaybe ts
    a <- find ((==t) . attributeType) as
    case tail ts of
        []  -> return a
        ts' -> case a of
            Attribute     _ _   -> Nothing
            AttributeNest _ as' -> findAttribute ts' (AttributeList as')
