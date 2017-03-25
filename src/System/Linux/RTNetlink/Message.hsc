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

import Control.Monad (guard)
import Data.Monoid (mempty)
import Data.Int (Int32)
import Data.List (nub)
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

type TypeNumber     = Word16 -- ^ Message type for an 'NlMsgHdr'.
type NLFlags        = Word16 -- ^ Top-level flags for an 'NlMsgHdr'.

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
    emptyHeader = NLMsgErr 0 $ NLMsgHdr 0 0 0 0 0

-- | Class of things that can be used as second-level netlink headers.
class (Show h, Eq h, Sized h, Serialize h) => Header h where
    -- | Default header for a message, if none is specified.
    emptyHeader :: h
instance Header () where
    emptyHeader = ()

-- | Class of things that can be sent as messages.
class Header (MessageHeader m) => Message m where
    -- | The type of header to attach to the message.
    type MessageHeader m
    -- | Construct a header corresponding to a message. Defaults to `emptyHeader`.
    messageHeader :: m -> MessageHeader m
    messageHeader = const emptyHeader
    -- | Construct netlink attributes corresponding to a message. Defaults to `mempty`.
    messageAttrs  :: m -> AttributeList
    messageAttrs  = mempty
    -- | Produce an NLMessage suitable for sending over the wire.
    toNLMessage   ::
        m -> TypeNumber -> NLFlags -> SequenceNumber -> NLMessage (MessageHeader m)
    toNLMessage m = NLMessage (messageHeader m) (messageAttrs m)
    {-# MINIMAL #-}

-- | Class of 'Message's representing things that can be created.
class Message c => Create c where
    -- | The top-level type number associated with creating with this message.
    createTypeNumber :: c -> TypeNumber
    -- | Produce an NLMessage suitable for sending over the wire.
    createNLMessage  :: c -> SequenceNumber -> NLMessage (MessageHeader c)
    createNLMessage c = toNLMessage c (createTypeNumber c) flags
        where flags = #{const NLM_F_REQUEST | NLM_F_ACK | NLM_F_CREATE | NLM_F_EXCL}
    {-# MINIMAL createTypeNumber #-}

-- | Class of 'Message's representing things that can be destroyed.
class Message d => Destroy d where
    -- | The top-level type number associated with destroying with this
    -- message.
    destroyTypeNumber :: d -> TypeNumber
    -- | Produce an NLMessage suitable for sending over the wire.
    destroyNLMessage  :: d -> SequenceNumber -> NLMessage (MessageHeader d)
    destroyNLMessage d = toNLMessage d (destroyTypeNumber d) flags
        where flags = #{const NLM_F_REQUEST | NLM_F_ACK}
    {-# MINIMAL destroyTypeNumber #-}

-- | Class of 'Message's representing pairs of identifying messages and
-- quality that can be modified.
class Message id => Change id c where
    -- | The top-level type number associated with changing things with this
    -- message.
    changeTypeNumber :: id -> c -> TypeNumber
    -- | Construct a header from an identifier and a quality. Should probably
    -- use the identifying message's 'messageHeader'.
    changeHeader     :: id -> c -> MessageHeader id
    -- | Construct aattributes from an identifier and a quality. Should
    -- probably use the identifying message's 'messageAttrs'.
    changeAttrs      :: id -> c -> AttributeList
    -- | Produce an NLMessage suitable for sending over the wire.
    changeNLMessage  :: id -> c -> SequenceNumber -> NLMessage (MessageHeader id)
    changeNLMessage i c = 
        NLMessage (changeHeader i c) (changeAttrs i c) (changeTypeNumber i c) flags
        where flags  = #{const NLM_F_REQUEST | NLM_F_ACK}
    {-# MINIMAL changeTypeNumber, changeHeader, changeAttrs #-}

-- | Class of 'Message's that can serve as requests.
class Message r => Request r where
    -- | The top-level type number associated with requesting things with this
    -- message.
    requestTypeNumber :: r -> TypeNumber
    -- | The top-level flags associated with this request.
    requestNLFlags    :: r -> NLFlags
    requestNLFlags = const #{const NLM_F_REQUEST}
    -- | Produce an NLMessage suitable for sending over the wire.
    requestNLMessage  :: r -> SequenceNumber -> NLMessage (MessageHeader r)
    requestNLMessage r = toNLMessage r (requestTypeNumber r) (requestNLFlags r)
    {-# MINIMAL requestTypeNumber #-}

-- | The default request flags assume that the request identifies a single
-- entity. When requesting information for multiple entities, overload
-- 'requestNLFlags' with these.
dumpNLFlags :: NLFlags
dumpNLFlags = #{const NLM_F_REQUEST | NLM_F_DUMP}

-- | Class of things that can be received.
class Header (ReplyHeader r) => Reply r where
    -- | The type of header associated with this 'Reply'.
    type ReplyHeader r
    -- | The expected top-level type number(s) that mark a packet this reply
    -- can be parsed from.
    replyTypeNumbers :: r -> [TypeNumber]
    -- | Interpret a received NLMessage.
    fromNLMessage    :: NLMessage (ReplyHeader r) -> Maybe r
    -- | Like 'fromNLMessage', but checks to make sure the top-level type
    -- number is in 'replyTypeNumbers', first.
    {-# MINIMAL replyTypeNumbers, fromNLMessage #-}
instance Reply () where
    type ReplyHeader () = ()
    replyTypeNumbers () = []
    fromNLMessage    _  = Nothing
instance (Reply r, Reply s, ReplyHeader r ~ ReplyHeader s) => Reply (r,s) where
    type ReplyHeader (r,s) = ReplyHeader r
    replyTypeNumbers (r,s) = nub $ replyTypeNumbers r ++ replyTypeNumbers s
    fromNLMessage    m     = (,) <$> fromNLMessage m <*> fromNLMessage m
instance Reply C.Errno where
    type ReplyHeader C.Errno = NLMsgErr
    replyTypeNumbers _       = [#{const NLMSG_ERROR}]
    fromNLMessage            = Just . C.Errno . abs . fromIntegral . nleError . nlmHeader

fromNLMessage' :: Reply r => NLMessage (ReplyHeader r) -> Maybe r
fromNLMessage' m = do
    r <- fromNLMessage m
    guard $ nlmType m `elem` replyTypeNumbers r
    return r

-- Util

decodeMaybe :: Serialize a => S.ByteString -> Maybe a
decodeMaybe = either (const Nothing) Just . decode

runGetMaybe :: Get a -> S.ByteString -> Maybe a
runGetMaybe g = either (const Nothing) Just . runGet g
