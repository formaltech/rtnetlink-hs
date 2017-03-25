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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module System.Linux.RTNetlink.Address where

import Data.Serialize
import Data.Word (Word8, Word32)
import System.Socket.Family.Inet (InetAddress, inetAddressToTuple, inetAddressFromTuple)

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

-- | An address and netmask associated with an interface.
data IfInetAddress = IfInetAddress
    { ifInetAddress :: InetAddress -- ^ The ip4v address itself.
    , ifInetPrefix  :: Word8       -- ^ The netmask.
    , ifInetIfIndex :: Word32      -- ^ Index of the associated interface.
    } deriving (Show, Eq)
instance Message IfInetAddress where
    type MessageHeader IfInetAddress = IfAddrMsg
    messageHeader IfInetAddress {..} =
        IfAddrMsg #{const AF_INET} ifInetPrefix 0 0 ifInetIfIndex
    messageAttrs  IfInetAddress {..} = AttributeList
        [ Attribute #{const RTA_SRC} ip4
        , Attribute #{const RTA_DST} ip4
        ] where ip4 = runPut $ putInetAddress ifInetAddress
instance Create IfInetAddress where
    createTypeNumber = const #{const RTM_NEWADDR}

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
