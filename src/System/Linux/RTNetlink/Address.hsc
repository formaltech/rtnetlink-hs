{-|
Module      : System.Linux.RTNetlink.Address
Description : ADTs for creating, destroying, modifying, and getting info
              about layer-3 addresses.
Copyright   : (c) Formaltech Inc. 2017
License     : BSD3
Maintainer  : protob3n@gmail.com
Stability   : experimental
Portability : Linux
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module System.Linux.RTNetlink.Address
    ( IfInetAddress(..)
    , IfInet6Address(..)
    , IfIndex(..)
    , IfPrefix(..)
    , AnyInterface(..)
    , IfAddrMsg(..)
    -- * Re-exports
    , InetAddress
    , Inet6Address
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (guard)
import Data.Monoid (mempty)
import Data.Serialize (Serialize, Get, Putter, get, put, runPut)
import Data.Serialize (getWord32host, putWord32host, getWord8)
import Data.Serialize (putWord8, getWord16be, putWord16be)
import Data.Word (Word8, Word32)
import System.Socket.Family.Inet (InetAddress, inetAddressToTuple)
import System.Socket.Family.Inet (inetAddressFromTuple)
import System.Socket.Family.Inet6 (Inet6Address, inet6AddressToTuple)
import System.Socket.Family.Inet6 (inet6AddressFromTuple)

import System.Linux.RTNetlink.Message
import System.Linux.RTNetlink.Packet

#include <linux/if_addr.h>
#include <linux/rtnetlink.h>
#include <netinet/in.h>

-- | Construct a network-byte-order representation of an 'InetAddress'.
putInetAddress :: Putter InetAddress
putInetAddress i = putWord8 a >> putWord8 b >> putWord8 c >> putWord8 d
    where (a,b,c,d) = inetAddressToTuple i

-- | Parse a network-byte-order representation of an 'InetAddress'.
getInetAddress :: Get InetAddress
getInetAddress = inetAddressFromTuple <$> getTuple
    where getTuple = (,,,) <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8

instance Message InetAddress where
    type MessageHeader InetAddress = IfAddrMsg
    messageAttrs address = AttributeList
        [ Attribute #{const RTA_SRC} ipv4
        , Attribute #{const RTA_DST} ipv4
        ] where ipv4 = runPut $ putInetAddress address
instance Reply InetAddress where
    type ReplyHeader InetAddress = IfAddrMsg
    replyTypeNumbers _           = [#{const RTM_NEWADDR}]
    fromNLMessage NLMessage {..} = do
        let IfAddrMsg {..} = nlmHeader
        guard $ addrFamily == #{const AF_INET}
        attr <- findAttribute [#{const RTA_DST}] nlmAttrs
        bs   <- attributeData attr
        runGetMaybe getInetAddress bs

-- | Construct a network-byte-order representation of an 'InetAddress'.
putInet6Address :: Putter Inet6Address
putInet6Address i = mapM_ putWord16be [a,b,c,d,e,f,g,h]
    where (a,b,c,d,e,f,g,h) = inet6AddressToTuple i

-- | Parse a network-byte-order representation of an 'Inet6Address'.
getInet6Address :: Get Inet6Address
getInet6Address = inet6AddressFromTuple <$> getTuple
    where
    getTuple = (,,,,,,,)
        <$> getWord16be
        <*> getWord16be
        <*> getWord16be
        <*> getWord16be
        <*> getWord16be
        <*> getWord16be
        <*> getWord16be
        <*> getWord16be

instance Message Inet6Address where
    type MessageHeader Inet6Address = IfAddrMsg
    messageAttrs address = AttributeList
        [ Attribute #{const RTA_SRC} ipv6
        , Attribute #{const RTA_DST} ipv6
        ] where ipv6 = runPut $ putInet6Address address
instance Reply Inet6Address where
    type ReplyHeader Inet6Address = IfAddrMsg
    replyTypeNumbers _           = [#{const RTM_NEWADDR}]
    fromNLMessage NLMessage {..} = do
        let IfAddrMsg {..} = nlmHeader
        guard $ addrFamily == #{const AF_INET6}
        attr <- findAttribute [#{const RTA_DST}] nlmAttrs
        bs   <- attributeData attr
        runGetMaybe getInet6Address bs

-- | Interface wildcard. Use this to get information about all layer-3
-- interfaces.
data AnyInterface = AnyInterface
    deriving (Show, Eq)
instance Message AnyInterface where
    type MessageHeader AnyInterface = IfAddrMsg
    messageAttrs       AnyInterface = mempty
instance Request AnyInterface where
    requestTypeNumber = const #{const RTM_GETADDR}
    requestNLFlags    = const dumpNLFlags

-- | The index of a layer-3 interface.
newtype IfIndex = IfIndex {ifIndex :: Int}
    deriving (Show, Eq, Num, Ord)
instance Message IfIndex where
    type MessageHeader IfIndex = IfAddrMsg
    messageHeader (IfIndex ix) = IfAddrMsg 0 0 0 0 (fromIntegral ix)
instance Reply IfIndex where
    type ReplyHeader IfIndex = IfAddrMsg
    replyTypeNumbers _       = [#{const RTM_NEWADDR}]
    fromNLMessage            = Just . IfIndex . fromIntegral . addrIndex . nlmHeader

-- | A netmask in CIDR notation.
newtype IfPrefix = IfPrefix {ifPrefix :: Word8}
    deriving (Show, Eq, Num, Ord)
instance Message IfPrefix where
    type MessageHeader IfPrefix = IfAddrMsg
    messageHeader (IfPrefix p)  = IfAddrMsg 0 p 0 0 0
instance Reply IfPrefix where
    type ReplyHeader IfPrefix = IfAddrMsg
    replyTypeNumbers _        = [#{const RTM_NEWADDR}]
    fromNLMessage             = Just . IfPrefix . addrPrefix . nlmHeader

-- | An ipv4 address and netmask associated with an interface.
data IfInetAddress = IfInetAddress
    { ifInetAddress :: InetAddress -- ^ The ip4v address itself.
    , ifInetPrefix  :: IfPrefix    -- ^ The netmask in CIDR notation.
    , ifInetIfIndex :: IfIndex     -- ^ Index of the associated interface.
    } deriving (Show, Eq)
instance Message IfInetAddress where
    type MessageHeader IfInetAddress = IfAddrMsg
    messageAttrs  IfInetAddress {..} = messageAttrs ifInetAddress
    messageHeader IfInetAddress {..} = IfAddrMsg
        { addrFamily = #{const AF_INET}
        , addrPrefix = ifPrefix ifInetPrefix
        , addrFlags  = 0
        , addrScope  = 0
        , addrIndex  = fromIntegral $ ifIndex ifInetIfIndex
        }
instance Create IfInetAddress where
    createTypeNumber = const #{const RTM_NEWADDR}
instance Destroy IfInetAddress where
    destroyTypeNumber = const #{const RTM_DELADDR}
instance Reply IfInetAddress where
    type ReplyHeader IfInetAddress = IfAddrMsg
    replyTypeNumbers _             = [#{const RTM_NEWADDR}]
    fromNLMessage    m             =
        IfInetAddress <$> fromNLMessage m <*> fromNLMessage m <*> fromNLMessage m

-- | An ipv6 address and netmask associated with an interface.
data IfInet6Address = IfInet6Address
    { ifInet6Address :: Inet6Address -- ^ The ip4v address itself.
    , ifInet6Prefix  :: IfPrefix     -- ^ The netmask in CIDR notation.
    , ifInet6IfIndex :: IfIndex      -- ^ Index of the associated interface.
    } deriving (Show, Eq)
instance Message IfInet6Address where
    type MessageHeader IfInet6Address = IfAddrMsg
    messageAttrs  IfInet6Address {..} = messageAttrs ifInet6Address
    messageHeader IfInet6Address {..} = IfAddrMsg
        { addrFamily = #{const AF_INET6}
        , addrPrefix = ifPrefix ifInet6Prefix
        , addrFlags  = 0
        , addrScope  = 0
        , addrIndex  = fromIntegral $ ifIndex ifInet6IfIndex
        }
instance Create IfInet6Address where
    createTypeNumber = const #{const RTM_NEWADDR}
instance Destroy IfInet6Address where
    destroyTypeNumber = const #{const RTM_DELADDR}
instance Reply IfInet6Address where
    type ReplyHeader IfInet6Address = IfAddrMsg
    replyTypeNumbers _             = [#{const RTM_NEWADDR}]
    fromNLMessage    m             =
        IfInet6Address <$> fromNLMessage m <*> fromNLMessage m <*> fromNLMessage m

-- | The header corresponding to address messages, based on 'struct ifaddrmsg'
-- from 'linux/if_addr.h'.
data IfAddrMsg = IfAddrMsg
    { addrFamily :: Word8  -- ^ Address family (AF_* from @sys/socket.h@)
    , addrPrefix :: Word8  -- ^ CIDR netmask for this address.
    , addrFlags  :: Word8  -- ^ Operational flags for this address.
    , addrScope  :: Word8  -- ^ Address scope.
    , addrIndex  :: Word32 -- ^ Index of the associated interface.
    } deriving (Show, Eq)
instance Sized IfAddrMsg where
    size = const #{const sizeof(struct ifaddrmsg)}
instance Serialize IfAddrMsg where
    put IfAddrMsg {..} = do
        putWord8      addrFamily
        putWord8      addrPrefix
        putWord8      addrFlags
        putWord8      addrScope
        putWord32host addrIndex
    get = IfAddrMsg
        <$> getWord8
        <*> getWord8
        <*> getWord8
        <*> getWord8
        <*> getWord32host
instance Header IfAddrMsg where
    emptyHeader = IfAddrMsg 0 0 0 0 0
