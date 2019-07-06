{-|
Module      : System.Linux.RTNetlink.Message
Description : High-level classes and ADTs for constructing netlink messages.
Copyright   : (c) Formaltech Inc. 2017
License     : BSD3
Maintainer  : protob3n@gmail.com
Stability   : experimental
Portability : Linux

A netlink packet contains a top-level header (@struct nlmsghdr@ from
@linux/netlink.h@), a second-level header that depends on the message type
(e.g., @struct ifinfomsg@ from @linux/if_link.h@), and a possibly nested
collection of attributes (see "System.Linux.RTNetlink.Packet").

The way to create a netlink packet in RTNetlink is to instantiate either the
'Message' or the 'Reply' class, which entails specifying what the header type
should be. You can then instantiate any of the 'Create', 'Destroy', 'Change',
or 'Request' classes to indicate which kinds of actions the message can be used
to perform.
-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module System.Linux.RTNetlink.Message where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad (guard)
import Data.Int (Int32)
import Data.List (nub)
import Data.Monoid (mempty, (<>))
import Data.Serialize
import Data.Word (Word16, Word32)
import qualified Data.ByteString as S
import qualified Foreign.C.Error as C

import System.Linux.RTNetlink.Packet

#include <linux/netlink.h>

-- | Sequence number for an 'NlMsgHdr'.
type SequenceNumber = Word32

-- | Get the sequence number of a message started by an 'NLMsgHdr'.
sequenceNumber :: S.ByteString -> SequenceNumber
sequenceNumber = either (const 0) nlMsgSeqNum . decode

type TypeNumber = Word16 -- ^ Message type for an 'NlMsgHdr'.
type NLFlags    = Word16 -- ^ Top-level flags for an 'NlMsgHdr'.

-- High-level representation of a netlink packet.
data NLMessage header = NLMessage
    { nlmHeader :: header         -- ^ The secondary header, determined by type.
    , nlmAttrs  :: AttributeList  -- ^ The message's 'Attribute's.
    , nlmType   :: TypeNumber     -- ^ Top-level message type.
    , nlmFlags  :: NLFlags        -- ^ Top-level message flags.
    , nlmSeqNum :: SequenceNumber -- ^ Message sequence number.
    } deriving (Show, Eq)
instance Sized header => Sized (NLMessage header) where
    size NLMessage {..} = size nlmAttrs + size nlmHeader + #{const sizeof(struct nlmsghdr)}
instance (Sized header, Serialize header) => Serialize (NLMessage header) where
    put m@(NLMessage {..}) = do
        put $ NLMsgHdr (size m) nlmType nlmFlags nlmSeqNum 0
        put $ nlmHeader
        put $ nlmAttrs
    get = do
        NLMsgHdr {..} <- get
        header        <- get
        attributes    <- get
        return $ NLMessage header attributes nlMsgType nlMsgFlags nlMsgSeqNum

-- | The header of an error sent in response to a bad netlink message. The
-- numeric values correspond to negated values from "Foreign.C.Error". Try
-- running @man 3 errno@ for more information.
data NLMsgErr = NLMsgErr
    { nleError  :: Int32    -- ^ Negated numeric error code.
    , nleHeader :: NLMsgHdr -- ^ The header of the offending message.
    } deriving (Show, Eq)
instance Sized NLMsgErr where
    size              = const #{const sizeof(struct nlmsgerr)}
instance Serialize NLMsgErr where
    put NLMsgErr {..} = putInt32host nleError >> put nleHeader
    get               = NLMsgErr <$> getInt32le <*> get
instance Header NLMsgErr where
    type HeaderPart NLMsgErr = NLMsgErrPart
    fromHeaderParts = foldr modify emptyHeader
        where
        modify (NLMsgErrError e)  h = h {nleError = e}
        modify (NLMsgErrHeader g) h = h {nleHeader = g}
    emptyHeader = NLMsgErr 0 $ NLMsgHdr 0 0 0 0 0
instance ReplyMessageHeader NLMsgErr where
    replyTypeNumbers _ = [#{const NLMSG_ERROR}]

data NLMsgErrPart
    = NLMsgErrError  Int32
    | NLMsgErrHeader NLMsgHdr
    deriving (Show, Eq)

-- | Class of things that can be used as second-level netlink headers.
class (Show h, Eq h, Sized h, Serialize h) => Header h where
    -- | Components for a 'Header', so they can be combined.
    type HeaderPart h
    -- | How to construct a 'Header' from a list of @HeaderPart@s. An empty list
    -- should correspond to 'emptyHeader'.
    fromHeaderParts :: [HeaderPart h] -> h
    -- | Default header for a message, if none is specified.
    emptyHeader :: h
instance Header () where
    type HeaderPart () = ()
    fromHeaderParts    = mempty
    emptyHeader        = ()

-- | Class of headers that can be used to create things.
class Header h => CreateMessageHeader h where
    -- | The top-level type number associated with create messages with this
    -- header.
    createTypeNumber :: h -> TypeNumber

-- | Class of headers that can be used to destroy things.
class Header h => DestroyMessageHeader h where
    -- | The top-level type number associated with destroy messages with this
    -- header.
    destroyTypeNumber :: h -> TypeNumber

-- | Class of headers that can be used to change things.
class Header h => ChangeMessageHeader h where
    -- | The top-level type number associated with change messages with this
    -- header.
    changeTypeNumber :: h -> TypeNumber

-- | Class of headers that can be used to request things.
class Header h => RequestMessageHeader h where
    -- | The top-level type number associated with request messages with this
    -- header.
    requestTypeNumber :: h -> TypeNumber

-- | Class of headers that can be received in reply messages.
class Header h => ReplyMessageHeader h where
    -- | The expected top-level type number(s) that mark a packet replies with
    -- this header can be parsed from.
    replyTypeNumbers :: h -> [TypeNumber]
instance ReplyMessageHeader () where
    replyTypeNumbers () = []

-- | Class of things that can be sent as messages.
class Header (MessageHeader m) => Message m where
    -- | The type of header to attach to the message.
    type MessageHeader m
    -- | Parts to construct a header corresponding to a message. Defaults
    -- to @[]@.
    messageHeaderParts :: m -> [HeaderPart (MessageHeader m)]
    messageHeaderParts = mempty
    -- | Construct netlink attributes corresponding to a message. Defaults
    -- to @[]@.
    messageAttrs :: m -> AttributeList
    messageAttrs = mempty
    {-# MINIMAL #-}
instance (Message m, Message n, MessageHeader m ~ MessageHeader n)
    => Message (m,n) where
    type MessageHeader (m,n) = MessageHeader m
    messageHeaderParts (m,n) = messageHeaderParts m <> messageHeaderParts n
    messageAttrs       (m,n) = messageAttrs m <> messageAttrs n

-- | Produce a 'MessageHeader' from a 'Message' using 'messageHeaderParts'.
messageHeader :: Message m => m -> MessageHeader m
messageHeader = fromHeaderParts . messageHeaderParts

-- | Produce an 'NLMessage' suitable for sending over the wire.
toNLMessage :: Message m => m -> (MessageHeader m -> TypeNumber)
    -> NLFlags -> SequenceNumber -> NLMessage (MessageHeader m)
toNLMessage m typeNumber = NLMessage header (messageAttrs m) (typeNumber header)
    where header = messageHeader m

-- | Class of 'Message's representing things that can be created.
class (Message c, CreateMessageHeader (MessageHeader c)) => Create c
instance {-# Overlappable #-} (Create c, Create d,
    MessageHeader c ~ MessageHeader d) => Create (c,d)

-- | Produce an NLMessage suitable for sending over the wire.
createNLMessage :: Create c => c -> SequenceNumber -> NLMessage (MessageHeader c)
createNLMessage c = toNLMessage c createTypeNumber flags
    where flags = #{const NLM_F_REQUEST | NLM_F_ACK | NLM_F_CREATE | NLM_F_EXCL}

-- | Class of 'Message's representing things that can be destroyed.
class (Message d, DestroyMessageHeader (MessageHeader d)) => Destroy d
instance (Destroy d, Destroy e, MessageHeader d ~ MessageHeader e)
    => Destroy (d,e)

-- | Produce an NLMessage suitable for sending over the wire.
destroyNLMessage :: Destroy d => d -> SequenceNumber -> NLMessage (MessageHeader d)
destroyNLMessage d = toNLMessage d destroyTypeNumber flags
    where flags = #{const NLM_F_REQUEST | NLM_F_ACK}

-- | Class of 'Message's representing pairs of identifying messages and
-- quality that can be modified.
class (Message id, ChangeMessageHeader (MessageHeader id)) => Change id c where
    -- | Construct a list of 'HeaderPart's from an identifier and a quality. By
    -- default, use the identifying message's 'messageHeaderParts'.
    changeHeaderParts :: id -> c -> [HeaderPart (MessageHeader id)]
    changeHeaderParts i _ = messageHeaderParts i
    -- | Construct an 'AttributeList' from an identifier and a quality. By
    -- default, use the identifying message's 'messageAttrs'.
    changeAttrs :: id -> c -> AttributeList
    changeAttrs i _ = messageAttrs i
    {-# MINIMAL #-}
instance (Change id c, Change id d) => Change id (c,d) where
    changeHeaderParts id (c,d) =
        changeHeaderParts id c <> changeHeaderParts id d
    changeAttrs id (c,d) = changeAttrs id c <> changeAttrs id d
instance (Change id1 c, Change id2 c, MessageHeader id1 ~ MessageHeader id2)
    => Change (id1,id2) c where
    changeHeaderParts (id1,id2) c =
        changeHeaderParts id1 c <> changeHeaderParts id2 c
    changeAttrs (id1,id2) c = changeAttrs id1 c <> changeAttrs id2 c

-- | Produce an NLMessage suitable for sending over the wire.
changeNLMessage :: Change id c => id -> c -> SequenceNumber
    -> NLMessage (MessageHeader id)
changeNLMessage i c = 
    NLMessage header (changeAttrs i c) (changeTypeNumber header) flags
    where
    header = fromHeaderParts $ changeHeaderParts i c
    flags  = #{const NLM_F_REQUEST | NLM_F_ACK}

-- | Class of 'Message's that can serve as requests.
class (Message r, RequestMessageHeader (MessageHeader r)) => Request r where
    -- | The top-level flags associated with this request.
    requestNLFlags :: r -> ChangeFlags NLFlags
    {-# MINIMAL requestNLFlags #-}
instance (Request r, Request s, MessageHeader r ~ MessageHeader s)
    => Request (r,s) where
    -- | If either 'Request' instance demands a single 'Reply', any tuple
    -- containing it should also demand a single 'Reply'. Otherwise we combine
    -- the 'requestNLFlags' of each tuple element.
    requestNLFlags (r,s) = if rFlags == dumpOne r || sFlags == dumpOne s
        then dumpOne r
        else rFlags <> sFlags
        where
        rFlags = requestNLFlags r
        sFlags = requestNLFlags s

-- | Produce an 'NLMessage' suitable for sending over the wire.
requestNLMessage :: Request r => r -> SequenceNumber
    -> NLMessage (MessageHeader r)
requestNLMessage r = toNLMessage r requestTypeNumber flags
    where flags = applyChangeFlags' $ requestNLFlags r

-- | Top-level flags to indicate that calling 'dump' is expected to yield a
-- single 'Reply'.
dumpOne :: a -> ChangeFlags NLFlags
dumpOne = const $
    ChangeFlags #{const NLM_F_REQUEST} #{const NLM_F_REQUEST | NLM_F_DUMP}

-- | Top-level flags to indicate that calling 'dump' is expected to yield a
-- multiple 'Reply's.
dumpMany :: a -> ChangeFlags NLFlags
dumpMany = const $ setChangeFlags #{const NLM_F_REQUEST | NLM_F_DUMP}

-- | Class of things that can be received.
class ReplyMessageHeader (ReplyHeader r) => Reply r where
    -- | The type of header associated with this 'Reply'.
    type ReplyHeader r
    -- | Interpret a received NLMessage.
    fromNLMessage :: NLMessage (ReplyHeader r) -> Maybe r
    -- | Like 'fromNLMessage', but checks to make sure the top-level type
    -- number is in 'replyTypeNumbers', first.
    {-# MINIMAL fromNLMessage #-}
instance Reply () where
    type ReplyHeader () = ()
    fromNLMessage    _  = Nothing
instance Reply C.Errno where
    type ReplyHeader C.Errno = NLMsgErr
    fromNLMessage = Just . C.Errno . abs . fromIntegral . nleError . nlmHeader
instance Reply r => Reply (Maybe r) where
    type ReplyHeader (Maybe r) = ReplyHeader r
    fromNLMessage m = return $ fromNLMessage m
instance (Reply r, Reply s, ReplyHeader r ~ ReplyHeader s)
    => Reply (Either r s) where
    type ReplyHeader (Either r s) = ReplyHeader r
    fromNLMessage m = Left <$> fromNLMessage m <|> Right <$> fromNLMessage m
instance (Reply r, Reply s, ReplyHeader r ~ ReplyHeader s) => Reply (r,s) where
    type ReplyHeader (r,s) = ReplyHeader r
    fromNLMessage m = (,) <$> fromNLMessage m <*> fromNLMessage m

class (Request q, Reply r) => Dump q r
instance Request q => Dump q ()
instance Request q => Dump q C.Errno
instance (Request r, Reply r) => Dump r r
instance Dump q r => Dump q (Maybe r)
instance (Dump q r, Dump q s, ReplyHeader r ~ ReplyHeader s)
    => Dump q (Either r s)
instance (Dump q r1, Dump q r2, ReplyHeader r1 ~ ReplyHeader r2)
    => Dump q (r1,r2)
instance (Dump q1 r, Dump q2 r, MessageHeader q1 ~ MessageHeader q2)
    => Dump (q1,q2) r
instance {-# Overlapping #-} (Dump q1 r1, Dump q2 r2,
    MessageHeader q1 ~ MessageHeader q2, ReplyHeader r1 ~ ReplyHeader r2)
    => Dump (q1,q2) (r1,r2)
instance {-# Overlappable #-} (Request q, Reply r,
    MessageHeader q ~ ReplyHeader r) => Dump q r

fromNLMessage' :: Reply r => NLMessage (ReplyHeader r) -> Maybe r
fromNLMessage' m = do
    r <- fromNLMessage m
    guard $ nlmType m `elem` replyTypeNumbers (nlmHeader m)
    return r
