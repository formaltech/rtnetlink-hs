{-|
Module      : System.Linux.RTNetlink.Route
Description : ADTs for creating, destroying, modifying, and getting info
              about routes.
Copyright   : (c) Formaltech Inc. 2017
License     : BSD3
Maintainer  : protob3n@gmail.com
Stability   : experimental
Portability : Linux

= Example:

> tryRTNL $ dump AnyRoute >>= (\x -> liftIO $ mapM_ (print) (x :: [(RouteOIFIndex, RouteGateway)]))
> (RouteOIFIndex 2,RouteGateway InetAddress 172.17.1.1)
> Right ()

-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Linux.RTNetlink.Route (
    AnyRoute(..)
  , RouteAddress(..)
  , Route6Address(..)
  , RouteGateway(..)
  , Route6Gateway(..)
  , RouteOIFIndex(..)
  , RouteSrcLen(..)
  , RouteDstLen(..)
  , RouteTable(..)
  , RouteProtocol(..)
  , RouteType(..)
  , RouteInfo(..)
  , Route6Info(..)
  , RouteDeleted(..)
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (guard)
import Data.Monoid ((<>), mempty)
import Data.Serialize (Serialize, get, put, runPut)
import Data.Serialize (getWord32host, putWord32host, getWord8, putWord8)
import Data.Serialize (getInt32le, putInt32host)
import Data.Word (Word8, Word32)
import System.Socket.Family.Inet (InetAddress)
import System.Socket.Family.Inet6 (Inet6Address)

import System.Linux.RTNetlink.Message
import System.Linux.RTNetlink.Packet
import System.Linux.RTNetlink.Address (getInetAddress, putInetAddress)
import System.Linux.RTNetlink.Address (getInet6Address, putInet6Address)
import System.Linux.RTNetlink.Scope

import Foreign.C.Types (CInt)

#include <linux/rtnetlink.h>
#include <netinet/in.h>

newtype RouteAddress = RouteAddress InetAddress
  deriving (Show, Eq)

-- | Route destination IP address
instance Message RouteAddress where
    type MessageHeader RouteAddress = RtmMsg
    messageAttrs (RouteAddress address) = AttributeList
        [ Attribute #{const RTA_DST} ipv4
        ] where ipv4 = runPut $ putInetAddress address
instance Reply RouteAddress where
    type ReplyHeader RouteAddress = RtmMsg
    replyTypeNumbers _           = [#{const RTM_NEWROUTE}]
    fromNLMessage NLMessage {..} = do
        let RtmMsg {..} = nlmHeader
        guard $ rtmFamily == #{const AF_INET}
        dstAttr <- findAttribute [#{const RTA_DST}] nlmAttrs
        dstBs   <- attributeData dstAttr
        RouteAddress <$> runGetMaybe getInetAddress dstBs

-- | Route destination IPv6 address
newtype Route6Address = Route6Address Inet6Address
  deriving (Show, Eq)

instance Message Route6Address where
    type MessageHeader Route6Address = RtmMsg
    messageAttrs (Route6Address address) = AttributeList
        [ Attribute #{const RTA_DST} ipv6
        ] where ipv6 = runPut $ putInet6Address address
instance Reply Route6Address where
    type ReplyHeader Route6Address = RtmMsg
    replyTypeNumbers _           = [#{const RTM_NEWROUTE}]
    fromNLMessage NLMessage {..} = do
        let RtmMsg {..} = nlmHeader
        guard $ rtmFamily == #{const AF_INET6}
        attr <- findAttribute [#{const RTA_DST}] nlmAttrs
        bs   <- attributeData attr
        Route6Address <$> runGetMaybe getInet6Address bs

-- | Route gateway IP address
newtype RouteGateway = RouteGateway InetAddress
  deriving (Show, Eq)

instance Message RouteGateway where
    type MessageHeader RouteGateway = RtmMsg
    messageAttrs (RouteGateway address) = AttributeList
        [ Attribute #{const RTA_GATEWAY} ipv4
        ] where ipv4 = runPut $ putInetAddress address
instance Reply RouteGateway where
    type ReplyHeader RouteGateway = RtmMsg
    replyTypeNumbers _           = [#{const RTM_NEWROUTE}]
    fromNLMessage NLMessage {..} = do
        let RtmMsg {..} = nlmHeader
        guard $ rtmFamily == #{const AF_INET}
        attr <- findAttribute [#{const RTA_GATEWAY}] nlmAttrs
        bs   <- attributeData attr
        RouteGateway <$> runGetMaybe getInetAddress bs

-- | Route gateway IPv6 address
newtype Route6Gateway = Route6Gateway Inet6Address
  deriving (Show, Eq)

instance Message Route6Gateway where
    type MessageHeader Route6Gateway = RtmMsg
    messageAttrs (Route6Gateway address) = AttributeList
        [ Attribute #{const RTA_GATEWAY} ipv6
        ] where ipv6 = runPut $ putInet6Address address
instance Reply Route6Gateway where
    type ReplyHeader Route6Gateway = RtmMsg
    replyTypeNumbers _           = [#{const RTM_NEWROUTE}]
    fromNLMessage NLMessage {..} = do
        let RtmMsg {..} = nlmHeader
        guard $ rtmFamily == #{const AF_INET6}
        attr <- findAttribute [#{const RTA_GATEWAY}] nlmAttrs
        bs   <- attributeData attr
        Route6Gateway <$> runGetMaybe getInet6Address bs

-- | Route outgoing inteface ID
newtype RouteOIFIndex = RouteOIFIndex Int
  deriving (Show, Eq, Num)

instance Message RouteOIFIndex where
    type MessageHeader RouteOIFIndex = RtmMsg
    messageAttrs (RouteOIFIndex ix) = AttributeList
        [ Attribute #{const RTA_OIF} (runPut $ putInt32host $ fromIntegral ix) ]
instance Reply RouteOIFIndex where
    type ReplyHeader RouteOIFIndex = RtmMsg
    replyTypeNumbers _           = [#{const RTM_NEWROUTE}]
    fromNLMessage NLMessage {..} = do
        let RtmMsg {..} = nlmHeader
        attr <- findAttribute [#{const RTA_OIF}] nlmAttrs
        bs   <- attributeData attr
        RouteOIFIndex . fromIntegral <$> runGetMaybe getInt32le bs

-- | Route wildcard. Use this to get information about all routes
data AnyRoute = AnyRoute
    deriving (Show, Eq)
instance Message AnyRoute where
    type MessageHeader AnyRoute = RtmMsg
    messageAttrs       AnyRoute = mempty
instance Request AnyRoute where
    requestTypeNumber = const #{const RTM_GETROUTE}
    requestNLFlags    = const dumpNLFlags

 -- | Route destination length
newtype RouteDstLen = RouteDstLen Int
  deriving (Show, Eq, Num)

instance Message RouteDstLen where
    type MessageHeader RouteDstLen = RtmMsg
    messageHeader (RouteDstLen t) = emptyHeader { rtmDstLen = fromIntegral t}
instance Reply RouteDstLen where
    type ReplyHeader RouteDstLen = RtmMsg
    replyTypeNumbers _           = [#{const RTM_NEWROUTE}]
    fromNLMessage            = Just . fromIntegral . rtmDstLen . nlmHeader

-- | Route source length
newtype RouteSrcLen = RouteSrcLen Int
  deriving (Show, Eq, Num)

instance Message RouteSrcLen where
    type MessageHeader RouteSrcLen = RtmMsg
    messageHeader (RouteSrcLen t) = emptyHeader { rtmSrcLen = fromIntegral t}
instance Reply RouteSrcLen where
    type ReplyHeader RouteSrcLen = RtmMsg
    replyTypeNumbers _           = [#{const RTM_NEWROUTE}]
    fromNLMessage            = Just . fromIntegral . rtmSrcLen . nlmHeader

-- | Route TOS
newtype RouteTOS = RouteTOS Int
  deriving (Show, Eq, Num)

instance Message RouteTOS where
    type MessageHeader RouteTOS = RtmMsg
    messageHeader (RouteTOS t) = emptyHeader { rtmTOS = fromIntegral t}
instance Reply RouteTOS where
    type ReplyHeader RouteTOS = RtmMsg
    replyTypeNumbers _           = [#{const RTM_NEWROUTE}]
    fromNLMessage            = Just . fromIntegral . rtmTOS . nlmHeader

-- | Route table
data RouteTable =
    RouteTableUnspec
  | RouteTableCompat
  | RouteTableMain
  | RouteTableLocal
  | RouteTableMax
  | RouteTableID Word8
 deriving (Eq, Show)

newtype RouteTableEnum = RouteTableEnum { fromRouteTableEnum :: CInt }
 deriving (Eq, Show, Num)
#{enum RouteTableEnum, RouteTableEnum, RT_TABLE_UNSPEC, RT_TABLE_COMPAT, RT_TABLE_MAIN, RT_TABLE_LOCAL, RT_TABLE_MAX}

fromRouteTable :: RouteTable -> RouteTableEnum
fromRouteTable RouteTableUnspec = rtTableUnspec
fromRouteTable RouteTableCompat = rtTableCompat
fromRouteTable RouteTableMain = rtTableMain
fromRouteTable RouteTableLocal = rtTableLocal
fromRouteTable RouteTableMax = rtTableMax
fromRouteTable (RouteTableID x) = RouteTableEnum $ fromInteger $ fromIntegral x

toRouteTable :: RouteTableEnum -> RouteTable
toRouteTable x | x == rtTableUnspec = RouteTableUnspec
toRouteTable x | x == rtTableCompat = RouteTableCompat
toRouteTable x | x == rtTableMain = RouteTableMain
toRouteTable x | x == rtTableLocal = RouteTableLocal
toRouteTable x | x == rtTableMax = RouteTableMax
toRouteTable x | otherwise = RouteTableID $ fromIntegral $ fromRouteTableEnum x

instance Message RouteTable where
    type MessageHeader RouteTable = RtmMsg
    messageHeader t = emptyHeader { rtmTable = fromIntegral $ fromRouteTableEnum $ fromRouteTable t}

instance Reply RouteTable where
    type ReplyHeader RouteTable = RtmMsg
    replyTypeNumbers _       = [#{const RTM_NEWROUTE}]
    fromNLMessage            = Just . toRouteTable . fromIntegral . rtmTable . nlmHeader

-- | Routing protocol
data RouteProtocol =
    RouteProtocolUnspecified
  | RouteProtocolRedirect
  | RouteProtocolKernel
  | RouteProtocolBoot
  | RouteProtocolStatic
  | RouteProtocolGateD
  | RouteProtocolRA
  | RouteProtocolMRT
  | RouteProtocolZebra
  | RouteProtocolBird
  | RouteProtocolDNRouted
  | RouteProtocolXORP
  | RouteProtocolNTK
  | RouteProtocolDHCP
  | RouteProtocolMRouted
  | RouteProtocolBabel
 deriving (Eq, Show)

fromProtocol :: RouteProtocol -> CInt
fromProtocol RouteProtocolUnspecified = #{const RTPROT_UNSPEC}
fromProtocol RouteProtocolRedirect = #{const RTPROT_REDIRECT}
fromProtocol RouteProtocolKernel = #{const RTPROT_KERNEL}
fromProtocol RouteProtocolBoot = #{const RTPROT_BOOT}
fromProtocol RouteProtocolStatic = #{const RTPROT_STATIC}
fromProtocol RouteProtocolGateD = #{const RTPROT_GATED}
fromProtocol RouteProtocolRA = #{const RTPROT_RA}
fromProtocol RouteProtocolMRT = #{const RTPROT_MRT}
fromProtocol RouteProtocolZebra = #{const RTPROT_ZEBRA}
fromProtocol RouteProtocolBird = #{const RTPROT_BIRD}
fromProtocol RouteProtocolDNRouted = #{const RTPROT_DNROUTED}
fromProtocol RouteProtocolXORP = #{const RTPROT_XORP}
fromProtocol RouteProtocolNTK = #{const RTPROT_NTK}
fromProtocol RouteProtocolDHCP = #{const RTPROT_DHCP}
fromProtocol RouteProtocolMRouted = #{const RTPROT_MROUTED}
fromProtocol RouteProtocolBabel = #{const RTPROT_BABEL}

toProtocol :: CInt -> RouteProtocol
toProtocol #{const RTPROT_UNSPEC} = RouteProtocolUnspecified
toProtocol #{const RTPROT_REDIRECT} = RouteProtocolRedirect
toProtocol #{const RTPROT_KERNEL} = RouteProtocolKernel
toProtocol #{const RTPROT_BOOT} = RouteProtocolBoot
toProtocol #{const RTPROT_STATIC} = RouteProtocolStatic
toProtocol #{const RTPROT_GATED} = RouteProtocolGateD
toProtocol #{const RTPROT_RA} = RouteProtocolRA
toProtocol #{const RTPROT_MRT} = RouteProtocolMRT
toProtocol #{const RTPROT_ZEBRA} = RouteProtocolZebra
toProtocol #{const RTPROT_BIRD} = RouteProtocolBird
toProtocol #{const RTPROT_DNROUTED} = RouteProtocolDNRouted
toProtocol #{const RTPROT_XORP} = RouteProtocolXORP
toProtocol #{const RTPROT_NTK} = RouteProtocolNTK
toProtocol #{const RTPROT_DHCP} = RouteProtocolDHCP
toProtocol #{const RTPROT_MROUTED} = RouteProtocolMRouted
toProtocol #{const RTPROT_BABEL} = RouteProtocolBabel
toProtocol _ = error "Unknown protocol"

instance Message RouteProtocol where
    type MessageHeader RouteProtocol = RtmMsg
    messageHeader p = emptyHeader { rtmProtocol = fromIntegral $ fromProtocol p }

instance Reply RouteProtocol where
    type ReplyHeader RouteProtocol = RtmMsg
    replyTypeNumbers _       = [#{const RTM_NEWROUTE}]
    fromNLMessage            = Just . toProtocol . fromIntegral . rtmProtocol . nlmHeader

-- | Route scope
instance Message Scope where
    type MessageHeader Scope = RtmMsg
    messageHeader scope = emptyHeader { rtmScope = fromIntegral $ fromRtScopeEnum $ fromScope scope }

instance Reply Scope where
    type ReplyHeader Scope = RtmMsg
    replyTypeNumbers _       = [#{const RTM_NEWROUTE}]
    fromNLMessage            = Just . toScope . fromIntegral . rtmScope . nlmHeader

-- | Route type
newtype RouteTypeEnum = RtTypeEnum { fromRouteTypeEnum :: CInt }
 deriving (Eq, Show, Num)
#{enum RouteTypeEnum, RtTypeEnum,
  RTN_UNSPEC,
  RTN_UNICAST,
  RTN_LOCAL,
  RTN_BROADCAST,
  RTN_ANYCAST,
  RTN_MULTICAST,
  RTN_BLACKHOLE,
  RTN_UNREACHABLE,
  RTN_PROHIBIT,
  RTN_THROW,
  RTN_NAT,
  RTN_XRESOLVE}

data RouteType =
    RouteTypeUnspec
  | RouteTypeUnicast
  | RouteTypeLocal
  | RouteTypeBroadcast
  | RouteTypeAnycast
  | RouteTypeMulticast
  | RouteTypeBlackhole
  | RouteTypeUnreachable
  | RouteTypeProhibit
  | RouteTypeThrow
  | RouteTypeNat
  | RouteTypeXresolve
 deriving (Eq, Show)

fromRouteType :: RouteType -> RouteTypeEnum
fromRouteType RouteTypeUnspec = rtnUnspec
fromRouteType RouteTypeUnicast = rtnUnicast
fromRouteType RouteTypeLocal = rtnLocal
fromRouteType RouteTypeBroadcast = rtnBroadcast
fromRouteType RouteTypeAnycast = rtnAnycast
fromRouteType RouteTypeMulticast = rtnMulticast
fromRouteType RouteTypeBlackhole = rtnBlackhole
fromRouteType RouteTypeUnreachable = rtnUnreachable
fromRouteType RouteTypeProhibit = rtnProhibit
fromRouteType RouteTypeThrow = rtnThrow
fromRouteType RouteTypeNat = rtnNat
fromRouteType RouteTypeXresolve = rtnXresolve

toRouteType :: RouteTypeEnum -> RouteType
toRouteType x | x == rtnUnspec = RouteTypeUnspec
toRouteType x | x == rtnUnicast = RouteTypeUnicast
toRouteType x | x == rtnLocal = RouteTypeLocal
toRouteType x | x == rtnBroadcast = RouteTypeBroadcast
toRouteType x | x == rtnAnycast = RouteTypeAnycast
toRouteType x | x == rtnMulticast = RouteTypeMulticast
toRouteType x | x == rtnBlackhole = RouteTypeBlackhole
toRouteType x | x == rtnUnreachable = RouteTypeUnreachable
toRouteType x | x == rtnProhibit = RouteTypeProhibit
toRouteType x | x == rtnThrow = RouteTypeThrow
toRouteType x | x == rtnNat = RouteTypeNat
toRouteType x | x == rtnXresolve = RouteTypeXresolve
toRouteType _ = error "No such route type"

instance Message RouteType where
    type MessageHeader RouteType = RtmMsg
    messageHeader typ = emptyHeader { rtmType = fromIntegral $ fromRouteTypeEnum $ fromRouteType typ }

instance Reply RouteType where
    type ReplyHeader RouteType = RtmMsg
    replyTypeNumbers _       = [#{const RTM_NEWROUTE}]
    fromNLMessage            = Just . toRouteType . fromIntegral . rtmType . nlmHeader

-- | Complex IP route info
data RouteInfo = RouteInfo
  { routeDstLen :: RouteDstLen
  , routeSrcLen :: RouteSrcLen
  , routeTOS :: RouteTOS
  , routeOIFIndex :: RouteOIFIndex
  , routeTable :: RouteTable
  , routeProtocol :: RouteProtocol
  , routeScope :: Scope
  , routeType :: RouteType
  , routeDstAddress :: Maybe RouteAddress
  , routeGateway :: Maybe RouteGateway
  } deriving (Show, Eq)

instance Message RouteInfo where
    type MessageHeader RouteInfo = RtmMsg
    messageAttrs  RouteInfo {..} =
           messageAttrs routeOIFIndex
        <> maybe (mempty) (messageAttrs) routeDstAddress
        <> maybe (mempty) (messageAttrs) routeGateway
    messageHeader RouteInfo {..} = emptyHeader
        { rtmFamily = #{const AF_INET}
        , rtmDstLen = dstLen routeDstLen
        , rtmSrcLen = srcLen routeSrcLen
        , rtmTOS = tos routeTOS
        , rtmTable = fromIntegral $ fromRouteTableEnum $ fromRouteTable routeTable
        , rtmProtocol = fromIntegral $ fromProtocol routeProtocol
        , rtmScope = fromIntegral $ fromRtScopeEnum $ fromScope routeScope
        , rtmType = fromIntegral $ fromRouteTypeEnum $ fromRouteType routeType
        }
      where
        dstLen (RouteDstLen x) = fromIntegral x
        srcLen (RouteSrcLen x) = fromIntegral x
        tos    (RouteTOS x)    = fromIntegral x

instance Create RouteInfo where
    createTypeNumber = const #{const RTM_NEWROUTE}
instance Destroy RouteInfo where
    destroyTypeNumber = const #{const RTM_DELROUTE}
instance Reply RouteInfo where
    type ReplyHeader RouteInfo = RtmMsg
    replyTypeNumbers _             = [#{const RTM_NEWROUTE}]
    fromNLMessage    m             = do
        let RtmMsg {..} = nlmHeader m
        guard $ rtmFamily == #{const AF_INET}
        RouteInfo <$> fromNLMessage m
                  <*> fromNLMessage m
                  <*> fromNLMessage m
                  <*> fromNLMessage m
                  <*> fromNLMessage m
                  <*> fromNLMessage m
                  <*> fromNLMessage m
                  <*> fromNLMessage m
                  <*> maybeFromNLMessage m
                  <*> maybeFromNLMessage m

-- | Complex IPv6 route info
data Route6Info = Route6Info
  { route6DstLen :: RouteDstLen
  , route6SrcLen :: RouteSrcLen
  , route6TOS :: RouteTOS
  , route6OIFIndex :: RouteOIFIndex
  , route6Table :: RouteTable
  , route6Protocol :: RouteProtocol
  , route6Scope :: Scope
  , route6Type :: RouteType
  , route6DstAddress :: Maybe Route6Address
  , route6Gateway :: Maybe Route6Gateway
  } deriving (Show, Eq)

instance Message Route6Info where
    type MessageHeader Route6Info = RtmMsg
    messageAttrs  Route6Info {..} =
           messageAttrs route6OIFIndex
        <> maybe (mempty) (messageAttrs) route6DstAddress
        <> maybe (mempty) (messageAttrs) route6Gateway
    messageHeader Route6Info {..} = emptyHeader
        { rtmFamily = #{const AF_INET}
        , rtmDstLen = dstLen route6DstLen
        , rtmSrcLen = srcLen route6SrcLen
        , rtmTOS = tos route6TOS
        , rtmTable = fromIntegral $ fromRouteTableEnum $ fromRouteTable route6Table
        , rtmProtocol = fromIntegral $ fromProtocol route6Protocol
        , rtmScope = fromIntegral $ fromRtScopeEnum $ fromScope route6Scope
        , rtmType = fromIntegral $ fromRouteTypeEnum $ fromRouteType route6Type
        }
      where
        dstLen (RouteDstLen x) = fromIntegral x
        srcLen (RouteSrcLen x) = fromIntegral x
        tos    (RouteTOS x)    = fromIntegral x

instance Create Route6Info where
    createTypeNumber = const #{const RTM_NEWROUTE}
instance Destroy Route6Info where
    destroyTypeNumber = const #{const RTM_DELROUTE}
instance Reply Route6Info where
    type ReplyHeader Route6Info = RtmMsg
    replyTypeNumbers _             = [#{const RTM_NEWROUTE}]
    fromNLMessage    m             = do
        let RtmMsg {..} = nlmHeader m
        guard $ rtmFamily == #{const AF_INET6}
        Route6Info <$> fromNLMessage m
                  <*> fromNLMessage m
                  <*> fromNLMessage m
                  <*> fromNLMessage m
                  <*> fromNLMessage m
                  <*> fromNLMessage m
                  <*> fromNLMessage m
                  <*> fromNLMessage m
                  <*> maybeFromNLMessage m
                  <*> maybeFromNLMessage m

-- | The header corresponding to address messages, based on 'struct RtmMsg'
-- from 'linux/rtnetlink.h'.
data RtmMsg = RtmMsg
    { rtmFamily   :: Word8  -- ^ Address family of route (AF_* from @sys/socket.h@)
    , rtmDstLen   :: Word8  -- ^ Length of destination
    , rtmSrcLen   :: Word8  -- ^ Length  of source
    , rtmTOS      :: Word8  -- ^ TOS filter
    , rtmTable    :: Word8  -- ^ Routing table ID
    , rtmProtocol :: Word8  -- ^ Routing protocol
    , rtmScope    :: Word8  -- ^ Distance to the destination
    , rtmType     :: Word8  -- ^ Route type
    , rtmFlags    :: Word32 -- ^ Route flags
    } deriving (Show, Eq)
instance Sized RtmMsg where
    size = const #{const sizeof(struct rtmsg)}
instance Serialize RtmMsg where
    put RtmMsg {..} = do
        putWord8      rtmFamily
        putWord8      rtmDstLen
        putWord8      rtmSrcLen
        putWord8      rtmTOS
        putWord8      rtmTable
        putWord8      rtmProtocol
        putWord8      rtmScope
        putWord8      rtmType
        putWord32host rtmFlags
    get = RtmMsg
        <$> getWord8
        <*> getWord8
        <*> getWord8
        <*> getWord8
        <*> getWord8
        <*> getWord8
        <*> getWord8
        <*> getWord8
        <*> getWord32host
instance Header RtmMsg where
    emptyHeader = RtmMsg 0 0 0 0 0 0 0 0 0

newtype RouteDeleted a = RouteDeleted a
    deriving (Eq, Show)
instance Reply a => Reply (RouteDeleted a) where
    type ReplyHeader (RouteDeleted a)  = ReplyHeader a
    replyTypeNumbers _             = [#{const RTM_DELROUTE}]
    fromNLMessage    m =
          RouteDeleted <$> fromNLMessage m

-- EXAMPLES
-- default gateway
-- tryRTNL $ dump AnyRoute >>= (\x -> liftIO $ mapM_ (print) (x :: [(RouteOIFIndex, RouteGateway)]))
