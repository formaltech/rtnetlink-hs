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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module System.Linux.RTNetlink.Address
    ( IfInetAddress(..)
    , IfInet6Address(..)
    , IfIndex(..)
    , IfPrefix(..)
    , IfScope(..)
    , IfLabel(..)
    , Precedence(..)
    , DuplicateAddressDetection(..)
    , DuplicateAddressDetectionFlags(..)
    , Mip6Homing(..)
    , Preference(..)
    , Permanence(..)
    , PrefixRoute(..)
    , MulticastAutoJoin(..)
    , IfSeconds(..)
    , IfLifetime(..)
    , AnyInterface(..)
    , IfAddrMsg(..)
    -- * Re-exports
    , InetAddress
    , inetAddressFromTuple
    , inetAddressToTuple
    , Inet6Address
    , inet6AddressFromTuple
    , inet6AddressToTuple
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (throw)
import Control.Monad (guard, when)
import Data.Bits ((.&.))
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty, (<>))
import Data.Serialize (Serialize, Get, Putter, get, put, runPut)
import Data.Serialize (getWord32host, putWord32host, getWord8)
import Data.Serialize (putWord8, getWord16be, putWord16be)
import Data.String (IsString)
import Data.Word (Word8, Word32)
import System.Socket.Family.Inet (InetAddress, inetAddressToTuple)
import System.Socket.Family.Inet (inetAddressFromTuple)
import System.Socket.Family.Inet6 (Inet6Address, inet6AddressToTuple)
import System.Socket.Family.Inet6 (inet6AddressFromTuple)
import qualified Data.ByteString.Char8 as S
import qualified Foreign.C.Error as C

import System.Linux.RTNetlink.Message
import System.Linux.RTNetlink.Packet
import System.Linux.RTNetlink.Util

#include <linux/if_addr.h>
#include <linux/rtnetlink.h>
#include <net/if.h>
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
    fromNLMessage NLMessage {..} = do
        guard $ (addrFamily nlmHeader) == #{const AF_INET}
        findAttributeGet getInetAddress [#{const RTA_DST}] nlmAttrs

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
    fromNLMessage NLMessage {..} = do
        guard $ (addrFamily nlmHeader) == #{const AF_INET6}
        findAttributeGet getInet6Address [#{const RTA_DST}] nlmAttrs

-- | Interface wildcard. Use this to get information about all layer-3
-- interfaces.
data AnyInterface = AnyInterface
    deriving (Show, Eq)
instance Message AnyInterface where
    type MessageHeader AnyInterface = IfAddrMsg
    messageAttrs       AnyInterface = mempty
instance Request AnyInterface where
    requestNLFlags = dumpMany

-- | The index of a layer-3 interface.
newtype IfIndex = IfIndex {ifIndex :: Int}
    deriving (Show, Eq, Num, Ord, Real, Enum, Integral)
instance Message IfIndex where
    type MessageHeader IfIndex      = IfAddrMsg
    messageHeaderParts (IfIndex ix) = [IfAddrMsgIndex $ fromIntegral ix]
instance Reply IfIndex where
    type ReplyHeader IfIndex = IfAddrMsg
    fromNLMessage = Just . IfIndex . fromIntegral . addrIndex . nlmHeader

-- | A netmask in CIDR notation.
newtype IfPrefix = IfPrefix Word8
    deriving (Show, Eq, Num, Ord, Real, Enum, Integral)
instance Message IfPrefix where
    type MessageHeader IfPrefix     = IfAddrMsg
    messageHeaderParts (IfPrefix p) = [IfAddrMsgPrefix p]
instance Reply IfPrefix where
    type ReplyHeader IfPrefix = IfAddrMsg
    fromNLMessage = Just . fromIntegral . addrPrefix . nlmHeader

-- | Precedence for address on its link. The first address created on a subnet
-- is 'Primary', and each subsequent one is 'Secondary'. (This is similar to an
-- "alias" address in @ifconfig@.) By default, linux has the (maybe
-- counterintuitive) behavior that, when the primary address on a subnet is
-- deleted, all secondary interfaces are deleted as well. To change this
-- behavior, you can set @net.ipv{4|6}.conf.<dev>.promote_secondaries = 1@ in
-- @sysctl@.
data Precedence = Primary | Secondary
    deriving (Show, Eq)
instance Reply Precedence where
    type ReplyHeader Precedence = IfAddrMsg
    fromNLMessage m = Just $
        if (addrFlags . nlmHeader) m .&. #{const IFA_F_SECONDARY} == 0
            then Primary
            else Secondary

-- | Whether this IPv6 address should send duplicate address detection packets
-- (see RFC2462). Default is 'DadEnabled'. This flag only makes sense for IPv6
-- addresses, but the kernel is perfectly happy to let you set it on IPv4 ones,
-- with no effect.
data DuplicateAddressDetection = DadEnabled | DadDisabled
    deriving (Show, Eq)
instance Reply DuplicateAddressDetection where
    type ReplyHeader DuplicateAddressDetection = IfAddrMsg
    fromNLMessage m = Just $
        if (addrFlags . nlmHeader) m .&. #{const IFA_F_NODAD} == 0
            then DadEnabled
            else DadDisabled
instance Message DuplicateAddressDetection where
    type MessageHeader DuplicateAddressDetection = IfAddrMsg
    messageHeaderParts d =
        [ IfAddrMsgFlags $ ChangeFlags
            { cfFlags = if d == DadEnabled then 0 else #{const IFA_F_NODAD}
            , cfMask  = #{const IFA_F_NODAD}
            }
        ]
    messageAttrs d = AttributeList [ word32AttrPart #{const IFA_FLAGS} f m ]
        where
        f = if d == DadEnabled then 0 else #{const IFA_F_NODAD}
        m = #{const IFA_F_NODAD}
instance (Create c, MessageHeader c ~ IfAddrMsg)
    => Create (c, DuplicateAddressDetection)
instance (Create c, MessageHeader c ~ IfAddrMsg)
    => Create (DuplicateAddressDetection, c)

-- | Flags for IPv6 duplicate address detection. See RFC4862.
data DuplicateAddressDetectionFlags = DuplicateAddressDetectionFlags
    { dadOptimistic :: Bool
    -- ^ Whether to use this address for neighbor dicovery and receiving frames
    -- when it is in a tentative state (i.e., DAD has not yet succeeded). It is
    -- dis-preferred for source address selection, like a deprecated address.
    -- See RFC4429.
    , dadTentative  :: Bool
    -- ^ Indicates that DAD has not yet succeeded. This address will not be used
    -- for neighbor discovery unless 'dadOptimistic' is also set.
    , dadFailed     :: Bool
    -- ^ Indicates that duplicate address detection failed on this address.
    } deriving (Show, Eq)
instance Reply DuplicateAddressDetectionFlags where
    type ReplyHeader DuplicateAddressDetectionFlags = IfAddrMsg
    fromNLMessage m = Just $ DuplicateAddressDetectionFlags
        { dadOptimistic = flags .&. #{const IFA_F_OPTIMISTIC} /= 0
        , dadTentative  = flags .&. #{const IFA_F_TENTATIVE}  /= 0
        , dadFailed     = flags .&. #{const IFA_F_DADFAILED}  /= 0
        } where flags = addrFlags $ nlmHeader m

-- | Home address for IPv6. Used in Mobility for IPv6 (MIP6), which allows a
-- device to use its home address on mobile networks. See RFC6275.
data Mip6Homing = Home | NotHome deriving (Show, Eq)
instance Reply Mip6Homing where
    type ReplyHeader Mip6Homing = IfAddrMsg
    fromNLMessage m = Just $
        if (addrFlags . nlmHeader) m .&. #{const IFA_F_HOMEADDRESS} == 0
            then Home
            else NotHome
instance Message Mip6Homing where
    type MessageHeader Mip6Homing = IfAddrMsg
    messageHeaderParts h =
        [ IfAddrMsgFlags $ ChangeFlags
            { cfFlags = if h == NotHome then 0 else #{const IFA_F_HOMEADDRESS}
            , cfMask  = #{const IFA_F_HOMEADDRESS}
            }
        ]
    messageAttrs h = AttributeList [ word32AttrPart #{const IFA_FLAGS} f m ]
        where
        f = if h == Home then #{const IFA_F_HOMEADDRESS} else 0
        m = #{const IFA_F_HOMEADDRESS}

-- | Indicates an address that is past its preferred lifetime. A deprecated
-- address will be dis-preferred for source address selection.
data Preference = Prefered | Deprecated deriving (Show, Eq)
instance Reply Preference where
    type ReplyHeader Preference = IfAddrMsg
    fromNLMessage m = Just $
        if (addrFlags . nlmHeader) m .&. #{const IFA_F_DEPRECATED} == 0
            then Prefered
            else Deprecated

-- | A 'Permanent' IPv6 address is one that was explicitly created. A 'Dynamic'
-- address is one that was auto-generated, e.g. by SLAAC.
data Permanence = Permanent | Dynamic deriving (Show, Eq)
instance Reply Permanence where
    type ReplyHeader Permanence = IfAddrMsg
    fromNLMessage m = Just $
        if (addrFlags . nlmHeader) m .&. #{const IFA_F_PERMANENT} == 0
            then Dynamic
            else Permanent

-- | Whether to automatically add a route based on the prefix when the address
-- is added.
data PrefixRoute = PREnabled | PRDisabled
    deriving (Show, Eq)
instance Reply PrefixRoute where
    type ReplyHeader PrefixRoute = IfAddrMsg
    fromNLMessage m = Just . fromMaybe PREnabled $ do
        f <- findAttributeGet getWord32host [#{const IFA_FLAGS}] $ nlmAttrs m
        return $ if f .&. #{const IFA_F_NOPREFIXROUTE} == (0::Word32)
            then PREnabled
            else PRDisabled
instance Message PrefixRoute where
    type MessageHeader PrefixRoute = IfAddrMsg
    messageAttrs h = AttributeList [ word32AttrPart #{const IFA_FLAGS} f m ]
        where
        f = if h == PREnabled then 0 else #{const IFA_F_NOPREFIXROUTE}
        m = #{const IFA_F_NOPREFIXROUTE}
instance (Create c, MessageHeader c ~ IfAddrMsg) => Create (c, PrefixRoute)
instance (Create c, MessageHeader c ~ IfAddrMsg) => Create (PrefixRoute, c)

-- | Enable joining multicast groups when connected to a switch that does IGMP
-- snooping. Only sensible on multicast addresses.
data MulticastAutoJoin = AutoJoin | NoAutoJoin
    deriving (Show, Eq)
instance Reply MulticastAutoJoin where
    type ReplyHeader MulticastAutoJoin = IfAddrMsg
    fromNLMessage m = Just . fromMaybe NoAutoJoin $ do
        f <- findAttributeGet getWord32host [#{const IFA_FLAGS}] $ nlmAttrs m
        return $ if f .&. #{const IFA_F_MCAUTOJOIN} == (0::Word32)
            then NoAutoJoin
            else AutoJoin
instance Message MulticastAutoJoin where
    type MessageHeader MulticastAutoJoin = IfAddrMsg
    messageAttrs h = AttributeList [ word32AttrPart #{const IFA_FLAGS} f m ]
        where
        f = if h == NoAutoJoin then 0 else #{const IFA_F_MCAUTOJOIN}
        m = #{const IFA_F_MCAUTOJOIN}

data IfSeconds = IfSeconds Word32 | IfForever deriving (Show, Eq)
instance Ord IfSeconds where
    IfSeconds s `compare` IfSeconds t = s `compare` t
    IfForever   `compare` _           = GT
    _           `compare` IfForever   = LT
instance Serialize IfSeconds where
    put (IfSeconds s) = putWord32host s
    put IfForever     = putWord32host oneBits
    get = getWord32host >>= \s ->
        return $ if s == oneBits then IfForever else IfSeconds s

-- | The lifetime of this address. The address will be in a 'Prefered' state for
-- 'ifPrefered' seconds, after which it will be 'Deprecated'. After 'ifValid'
-- seconds, the address will be removed. 
data IfLifetime = IfLifetime
    { ifPrefered :: IfSeconds
    , ifValid    :: IfSeconds
    } deriving (Show, Eq)
instance Reply IfLifetime where
    type ReplyHeader IfLifetime = IfAddrMsg
    fromNLMessage = Just . fromMaybe (IfLifetime IfForever IfForever)
        . findAttributeGet getLifetime [#{const IFA_CACHEINFO}] . nlmAttrs
        where
        secsFromWord32 s = if s == oneBits then IfForever else IfSeconds s
        getLifetime      = IfLifetime
            <$> get           -- lft_prefered
            <*> get           -- lft_valid
            <*  getWord32host -- cstamp
            <*  getWord32host -- tstamp
instance Message IfLifetime where
    type MessageHeader IfLifetime = IfAddrMsg
    messageAttrs l = AttributeList [Attribute #{const IFA_CACHEINFO} cacheinfo]
        where
        cacheinfo = runPut $ do
            when (ifPrefered l > ifValid l) . throw $
                userError "prefered lifetime must not be greater than valid lifetime"
            put $ ifPrefered l -- lft_prefered
            put $ ifValid    l -- lft_valid
            putWord32host 0    -- cstamp
            putWord32host 0    -- tstamp
instance (Create c, MessageHeader c ~ IfAddrMsg) => Create (c, IfLifetime)
instance (Create c, MessageHeader c ~ IfAddrMsg) => Create (IfLifetime, c)

-- According to @rtnetlink.h@, not so much a scope as a distance to the
-- destination. 'IfUniverse' is for address that talk to the world at large,
-- 'IfLink addresses talk to physically connected addresses, etc. 'IfSite'
-- is only for IPv6 addresses and deprecated, according to @ip-address(8)@,
-- but the kernel is happy to let you set it on IPv4 addresses.
--
-- Note that scope is determined automatically for IPv6 addresses, so the
-- kernel will ignore any scope you attach to a newly created address.
data IfScope
    = IfUniverse        -- ^ Destination located anywhere (default).
    | IfUserScope Word8 -- ^ User-defined scope; 0, 200, and 253-5 are reserved.
    | IfSite            -- ^ IPv6 address valid inside this site. Deprecated.
    | IfLink            -- ^ Destination on attached link.
    | IfHost            -- ^ Local address.
    | IfNowhere         -- ^ Destination doesn't exist.
    deriving (Show, Eq)
instance Reply IfScope where
    type ReplyHeader IfScope = IfAddrMsg
    fromNLMessage = Just . toScope . addrScope . nlmHeader
        where
        toScope #{const RT_SCOPE_UNIVERSE} = IfUniverse
        toScope #{const RT_SCOPE_SITE}     = IfSite
        toScope #{const RT_SCOPE_LINK}     = IfLink
        toScope #{const RT_SCOPE_HOST}     = IfHost
        toScope #{const RT_SCOPE_NOWHERE}  = IfNowhere
        toScope n                          = IfUserScope n
instance Message IfScope where
    type MessageHeader IfScope = IfAddrMsg
    messageHeaderParts = (:[]) . IfAddrMsgScope . fromScope
        where
        fromScope IfUniverse      = #{const RT_SCOPE_UNIVERSE}
        fromScope (IfUserScope n) = n
        fromScope IfSite          = #{const RT_SCOPE_SITE}
        fromScope IfLink          = #{const RT_SCOPE_LINK}
        fromScope IfHost          = #{const RT_SCOPE_HOST}
        fromScope IfNowhere       = #{const RT_SCOPE_NOWHERE}
instance (Create c, MessageHeader c ~ IfAddrMsg) => Create (c, IfScope)
instance (Create c, MessageHeader c ~ IfAddrMsg) => Create (IfScope, c)

-- | A textual label applied to an IPv4 address. Defaults to the 'LinkName' of
-- the parent link. Note that this is ignored/absent for IPv6 addresses.
newtype IfLabel = IfLabel S.ByteString
    deriving (Show, Eq, IsString)
instance Reply IfLabel where
    type ReplyHeader IfLabel     = IfAddrMsg
    fromNLMessage NLMessage {..} = IfLabel
        <$> findAttributeCString [#{const IFA_LABEL}] nlmAttrs
instance Message IfLabel where
    type MessageHeader IfLabel = IfAddrMsg
    messageAttrs  (IfLabel bs) = AttributeList
        [ cStringAttr #{const IFA_LABEL} $ S.take #{const IFNAMSIZ} bs ]
instance (Create c, MessageHeader c ~ IfAddrMsg) => Create (c, IfLabel)
instance (Create c, MessageHeader c ~ IfAddrMsg) => Create (IfLabel, c)

-- | An ipv4 address and netmask associated with an interface.
data IfInetAddress = IfInetAddress
    { ifInetAddress :: InetAddress -- ^ The ip4v address itself.
    , ifInetPrefix  :: IfPrefix    -- ^ The netmask in CIDR notation.
    , ifInetIfIndex :: IfIndex     -- ^ Index of the associated interface.
    } deriving (Show, Eq)
instance Message IfInetAddress where
    type MessageHeader IfInetAddress      = IfAddrMsg
    messageAttrs       IfInetAddress {..} = messageAttrs ifInetAddress
    messageHeaderParts IfInetAddress {..} =
        [ IfAddrMsgFamily #{const AF_INET}
        , IfAddrMsgPrefix $ fromIntegral ifInetPrefix
        , IfAddrMsgIndex . fromIntegral $ ifIndex ifInetIfIndex
        ]
instance Create IfInetAddress
instance Destroy IfInetAddress
instance Reply IfInetAddress where
    type ReplyHeader IfInetAddress = IfAddrMsg
    fromNLMessage    m             = do
        guard $ (addrFamily . nlmHeader) m == #{const AF_INET}
        IfInetAddress <$> fromNLMessage m <*> fromNLMessage m <*> fromNLMessage m

-- | An ipv6 address and netmask associated with an interface.
data IfInet6Address = IfInet6Address
    { ifInet6Address :: Inet6Address -- ^ The ip4v address itself.
    , ifInet6Prefix  :: IfPrefix     -- ^ The netmask in CIDR notation.
    , ifInet6IfIndex :: IfIndex      -- ^ Index of the associated interface.
    } deriving (Show, Eq)
instance Message IfInet6Address where
    type MessageHeader IfInet6Address      = IfAddrMsg
    messageAttrs       IfInet6Address {..} = messageAttrs ifInet6Address
    messageHeaderParts IfInet6Address {..} =
        [ IfAddrMsgFamily #{const AF_INET6}
        , IfAddrMsgPrefix $ fromIntegral ifInet6Prefix
        , IfAddrMsgIndex . fromIntegral $ ifIndex ifInet6IfIndex
        ]
instance Create IfInet6Address
instance Destroy IfInet6Address
instance Reply IfInet6Address where
    type ReplyHeader IfInet6Address = IfAddrMsg
    fromNLMessage    m              = do
        guard $ (addrFamily . nlmHeader) m == #{const AF_INET6}
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
    type HeaderPart IfAddrMsg = IfAddrMsgPart
    fromHeaderParts = toHeader . foldr modify (0,0,mempty,0,0)
        where
        toHeader                   (a,b,c,d,e) = IfAddrMsg a b (cfFlags c) d e
        modify (IfAddrMsgFamily a) (_,b,c,d,e) = (a, b, c,    d, e)
        modify (IfAddrMsgPrefix b) (a,_,c,d,e) = (a, b, c,    d, e)
        modify (IfAddrMsgFlags  f) (a,b,c,d,e) = (a, b, f<>c, d, e)
        modify (IfAddrMsgScope  d) (a,b,c,_,e) = (a, b, c,    d, e)
        modify (IfAddrMsgIndex  e) (a,b,c,d,_) = (a, b, c,    d, e)
    emptyHeader = IfAddrMsg 0 0 0 0 0
instance CreateMessageHeader IfAddrMsg where
    createTypeNumber = const #{const RTM_NEWADDR}
instance DestroyMessageHeader IfAddrMsg where
    destroyTypeNumber = const #{const RTM_DELADDR}
instance RequestMessageHeader IfAddrMsg where
    requestTypeNumber = const #{const RTM_GETADDR}
instance ReplyMessageHeader IfAddrMsg where
    replyTypeNumbers = const [#{const RTM_NEWADDR}]

data IfAddrMsgPart
    = IfAddrMsgFamily Word8
    | IfAddrMsgPrefix Word8
    | IfAddrMsgFlags  (ChangeFlags Word8)
    | IfAddrMsgScope  Word8
    | IfAddrMsgIndex  Word32
    deriving (Show, Eq)
