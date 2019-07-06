{-|
Module      : System.Linux.RTNetlink.Link
Description : ADTs for creating, destroying, modifying, and getting info
              about links.
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
module System.Linux.RTNetlink.Link where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad (guard)
import Data.Bits ((.&.))
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Serialize
import Data.String (IsString)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.ByteString as S

import System.Linux.RTNetlink.Message
import System.Linux.RTNetlink.Packet
import System.Linux.RTNetlink.Util

#include <linux/if_ether.h>
#include <linux/if_link.h>
#include <linux/rtnetlink.h>
#include <net/if.h>

-- | A link identified by its index.
newtype LinkIndex = LinkIndex Int
    deriving (Show, Eq, Num, Ord, Enum, Real, Integral)
instance Serialize LinkIndex where
    put ix = putWord32host $ fromIntegral ix
    get    = fromIntegral <$> getWord32host
instance Message LinkIndex where
    type MessageHeader LinkIndex      = IfInfoMsg
    messageHeaderParts (LinkIndex ix) = [IfInfoMsgIndex (fromIntegral ix)]
instance (Create c, MessageHeader c ~ IfInfoMsg) => Create (c,LinkIndex)
instance (Create c, MessageHeader c ~ IfInfoMsg) => Create (LinkIndex,c)
instance Destroy LinkIndex
instance Request LinkIndex where
    requestNLFlags = dumpOne
instance Reply LinkIndex where
    type ReplyHeader LinkIndex = IfInfoMsg
    fromNLMessage = Just . LinkIndex . fromIntegral . ifIndex . nlmHeader

-- | A link identified by its name.
newtype LinkName = LinkName S.ByteString
    deriving (Show, Eq, IsString)
instance Message LinkName where
    type MessageHeader LinkName = IfInfoMsg
    messageAttrs  (LinkName bs) = AttributeList
        [ cStringAttr #{const IFLA_IFNAME} $ S.take #{const IFNAMSIZ} bs ]
instance (Create c, MessageHeader c ~ IfInfoMsg) => Create (c,LinkName)
instance (Create c, MessageHeader c ~ IfInfoMsg) => Create (LinkName,c)
instance Change LinkIndex LinkName where
    changeAttrs n m = messageAttrs n <> messageAttrs m
instance Destroy LinkName
instance Request LinkName where
    requestNLFlags = dumpOne
instance Reply LinkName where
    type ReplyHeader LinkName = IfInfoMsg
    fromNLMessage NLMessage {..} =
        LinkName <$> findAttributeCString [#{const IFLA_IFNAME}] nlmAttrs

-- | An ethernet address.
data LinkEther = LinkEther Word8 Word8 Word8 Word8 Word8 Word8
    deriving Eq
instance Show LinkEther where
    show (LinkEther a b c d e f) = showMac a b c d e f
instance Serialize LinkEther where
    put (LinkEther a b c d e f) = mapM_ put [a,b,c,d,e,f]
    get = LinkEther <$> get <*> get <*> get <*> get <*> get <*> get
instance Message LinkEther where
    type MessageHeader LinkEther = IfInfoMsg
    messageAttrs e = AttributeList [Attribute #{const IFLA_ADDRESS} $ encode e]
instance (Create c, MessageHeader c ~ IfInfoMsg) => Create (c,LinkEther)
instance (Create c, MessageHeader c ~ IfInfoMsg) => Create (LinkEther,c)
instance Change LinkName LinkEther where
    changeAttrs n m = messageAttrs n <> messageAttrs m
instance Change LinkIndex LinkEther where
    changeAttrs n m = messageAttrs m
instance Reply LinkEther where
    type ReplyHeader LinkEther = IfInfoMsg
    fromNLMessage m = findAttributeDecode [#{const IFLA_ADDRESS}] $ nlmAttrs m

-- | An ethernet broadcast address.
data LinkBroadcastEther = LinkBroadcastEther Word8 Word8 Word8 Word8 Word8 Word8
    deriving Eq
instance Show LinkBroadcastEther where
    show (LinkBroadcastEther a b c d e f) = showMac a b c d e f
instance Serialize LinkBroadcastEther where
    put (LinkBroadcastEther a b c d e f) = mapM_ put [a,b,c,d,e,f]
    get = LinkBroadcastEther <$> get <*> get <*> get <*> get <*> get <*> get
instance Message LinkBroadcastEther where
    type MessageHeader LinkBroadcastEther = IfInfoMsg
    messageAttrs e = AttributeList [Attribute #{const IFLA_BROADCAST} $ encode e]
instance (Create c, MessageHeader c ~ IfInfoMsg) => Create (c,LinkBroadcastEther)
instance (Create c, MessageHeader c ~ IfInfoMsg) => Create (LinkBroadcastEther,c)
instance Change LinkName LinkBroadcastEther where
    changeAttrs n m = messageAttrs n <> messageAttrs m
instance Change LinkIndex LinkBroadcastEther where
    changeAttrs n m = messageAttrs m
instance Reply LinkBroadcastEther where
    type ReplyHeader LinkBroadcastEther = IfInfoMsg
    fromNLMessage m = findAttributeDecode [#{const IFLA_BROADCAST}] $ nlmAttrs m

-- | Link wildcard.
data AnyLink = AnyLink
    deriving (Show, Eq)
instance Message AnyLink where
    type MessageHeader AnyLink = IfInfoMsg
instance Request AnyLink where
    requestNLFlags = dumpMany

-- | The type of a link.
data LinkType
    = Dummy                       -- ^ A dummy interface.
    | Bridge                      -- ^ A bridge interface.
    | Dot1QVlan  LinkIndex VlanId -- ^ An 802.1Q vlan interface.
    | Dot1adVlan LinkIndex VlanId -- ^ An 802.1ad vlan interface.
    | NamedLinkType S.ByteString  -- ^ Specify the link type name as a string.
    deriving (Show, Eq)
instance Message LinkType where
    type MessageHeader LinkType = IfInfoMsg
    messageAttrs t = case t of
        Dummy            -> setTypeName "dummy"
        Bridge           -> setTypeName "bridge"
        NamedLinkType n  -> setTypeName n
        Dot1QVlan ix vid -> messageAttrs vid
            <> setVlanProto #{const ETH_P_8021Q}
            <> setTypeName "vlan"
            <> setVlanLink ix
        Dot1adVlan ix vid -> messageAttrs vid 
            <> setVlanProto #{const ETH_P_8021AD}
            <> setTypeName "vlan"
            <> setVlanLink ix
        where
        setTypeName n = AttributeList
            [ AttributeNest #{const IFLA_LINKINFO}
                [ cStringAttr #{const IFLA_INFO_KIND} n ]
            ]
        setVlanProto p = AttributeList
            [ AttributeNest #{const IFLA_LINKINFO}
                [ AttributeNest #{const IFLA_INFO_DATA}
                    -- Weirdly, the kernel seems to want the vlan proto in BE.
                    [ word16Attr #{const IFLA_VLAN_PROTOCOL} (byteSwap16 p) ]
                ]
            ]
        setVlanLink ix = AttributeList
            [ word32Attr #{const IFLA_LINK} (fromIntegral ix) ]
instance Create LinkType
instance Request LinkType where
    requestNLFlags = dumpMany
instance Reply LinkType where
    type ReplyHeader LinkType = IfInfoMsg
    fromNLMessage m@(NLMessage {..}) = do
        info <- findAttributeDecode [#{const IFLA_LINKINFO}] nlmAttrs
        typ  <- findAttributeCString [#{const IFLA_INFO_KIND}] info
        handleTypeName info typ
        where
        handleTypeName info t = case t of
            "dummy"  -> return Dummy
            "bridge" -> return Bridge
            "vlan"   -> handleVlan info
            _        -> return $ NamedLinkType t
        handleVlan info = do
            idata <- findAttributeDecode [#{const IFLA_INFO_DATA}] info
            proto <- findAttributeDecode [#{const IFLA_VLAN_PROTOCOL}] idata
            case (proto::Word16) of
                (#{const ETH_P_8021Q}) -> Dot1QVlan
                    <$> getVlanLink <*> fromNLMessage m
                (#{const ETH_P_8021AD}) -> Dot1adVlan
                    <$> getVlanLink <*> fromNLMessage m
                _ -> return $ NamedLinkType "vlan"
        getVlanLink = findAttributeDecode [#{const IFLA_LINK}] nlmAttrs

-- | Tag id for a vlan interface.
newtype VlanId = VlanId Word16
    deriving (Show, Eq, Num, Ord, Real, Enum, Integral)
instance Message VlanId where
    type MessageHeader VlanId = IfInfoMsg
    messageAttrs (VlanId vid) = AttributeList
        [ AttributeNest #{const IFLA_LINKINFO}
            [ AttributeNest #{const IFLA_INFO_DATA}
                [ word16Attr #{const IFLA_VLAN_ID} vid ]
            ]
        ]
instance Reply VlanId where
    type ReplyHeader VlanId = IfInfoMsg
    fromNLMessage NLMessage {..} = do
        info  <- findAttributeDecode [#{const IFLA_LINKINFO}] nlmAttrs
        idata <- findAttributeDecode [#{const IFLA_INFO_DATA}] info
        vid   <- findAttributeGet getWord16host [#{const IFLA_VLAN_ID}] idata
        return $ VlanId vid

-- | The master interface for this interface for this one. For example, a bridge
-- interface.
data LinkMaster = Master LinkIndex | NoMaster
    deriving (Show, Eq)
instance Message LinkMaster where
    type MessageHeader LinkMaster = IfInfoMsg
    messageAttrs (Master n) = AttributeList [word32Attr #{const IFLA_MASTER} $ fromIntegral n]
    messageAttrs NoMaster   = AttributeList [word32Attr #{const IFLA_MASTER} 0]
instance Reply LinkMaster where
    type ReplyHeader LinkMaster = IfInfoMsg
    fromNLMessage NLMessage {..} = Just . fromMaybe NoMaster $ do
        ix <- findAttributeDecode [#{const IFLA_MASTER}] nlmAttrs
        guard $ ix > 0
        return $ Master ix
instance Change LinkName LinkMaster where
    changeAttrs n m = messageAttrs n <> messageAttrs m
instance Change LinkIndex LinkMaster where
    changeAttrs n m = messageAttrs m

-- | The state of a link.
data LinkState = Up | Down
    deriving (Show, Eq)
instance Reply LinkState where
    type ReplyHeader LinkState = IfInfoMsg
    fromNLMessage m = Just $ if flag == 0 then Down else Up
        where flag = ifFlags (nlmHeader m) .&. #{const IFF_UP}
instance Change LinkName LinkState where
    changeHeaderParts n s =
        [ IfInfoMsgFlags $ ChangeFlags
            { cfFlags = if s == Up then #{const IFF_UP} else 0
            , cfMask  = #{const IFF_UP}
            }
        ]
instance Change LinkIndex LinkState where
    changeHeaderParts n s =
        [ IfInfoMsgIndex $ fromIntegral n
        , IfInfoMsgFlags $ ChangeFlags
            { cfFlags = if s == Up then #{const IFF_UP} else 0
            , cfMask  = #{const IFF_UP}
            }
        ]

-- | A 'Promiscuous' link accepts all frames at layer 2; a 'Chaste' one accepts
-- just those addressed to it and possibly ones sent to the broadcast address.
data LinkPromiscuity = Promiscuous | Chaste
    deriving (Show, Eq)
instance Reply LinkPromiscuity where
    type ReplyHeader LinkPromiscuity = IfInfoMsg
    fromNLMessage m = Just $ if flag == 0 then Chaste else Promiscuous
        where flag = ifFlags (nlmHeader m) .&. #{const IFF_PROMISC}
instance Change LinkName LinkPromiscuity where
    changeHeaderParts n s =
        [ IfInfoMsgFlags $ ChangeFlags
            { cfFlags = if s == Promiscuous then #{const IFF_PROMISC} else 0
            , cfMask  = #{const IFF_PROMISC}
            }
        ]
instance Change LinkIndex LinkPromiscuity where
    changeHeaderParts n s =
        [ IfInfoMsgIndex $ fromIntegral n
        , IfInfoMsgFlags $ ChangeFlags
            { cfFlags = if s == Promiscuous then #{const IFF_PROMISC} else 0
            , cfMask  = #{const IFF_PROMISC}
            }
        ]

-- | Whether to use ARP on the interface to resolve L3 addresses to L2 ones.
data LinkArp = Arp | NoArp
    deriving (Show, Eq)
instance Reply LinkArp where
    type ReplyHeader LinkArp = IfInfoMsg
    fromNLMessage m = Just $ if flag == 0 then Arp else NoArp
        where flag = ifFlags (nlmHeader m) .&. #{const IFF_NOARP}
instance Change LinkName LinkArp where
    changeHeaderParts n s =
        [ IfInfoMsgFlags $ ChangeFlags
            { cfFlags = if s == NoArp then #{const IFF_NOARP} else 0
            , cfMask  = #{const IFF_NOARP}
            }
        ]
instance Change LinkIndex LinkArp where
    changeHeaderParts n s =
        [ IfInfoMsgIndex $ fromIntegral n
        , IfInfoMsgFlags $ ChangeFlags
            { cfFlags = if s == NoArp then #{const IFF_NOARP} else 0
            , cfMask  = #{const IFF_NOARP}
            }
        ]

-- | Internal debug flag. If this is supported by the driver, it will generally
-- spew some extra information into @dmesg@.
data LinkDebug = Debug | NoDebug
    deriving (Show, Eq)
instance Reply LinkDebug where
    type ReplyHeader LinkDebug = IfInfoMsg
    fromNLMessage m = Just $ if flag == 0 then NoDebug else Debug
        where flag = ifFlags (nlmHeader m) .&. #{const IFF_DEBUG}
instance Change LinkName LinkDebug where
    changeHeaderParts _ s =
        [ IfInfoMsgFlags $ ChangeFlags
            { cfFlags = if s == Debug then #{const IFF_DEBUG} else 0
            , cfMask  = #{const IFF_DEBUG}
            }
        ]
instance Change LinkIndex LinkDebug where
    changeHeaderParts n s =
        [ IfInfoMsgIndex $ fromIntegral n
        , IfInfoMsgFlags $ ChangeFlags
            { cfFlags = if s == Debug then #{const IFF_DEBUG} else 0
            , cfMask  = #{const IFF_DEBUG}
            }
        ]

-- | Maximum transmission unit for a link. Note that some interface types, such
-- as 'Bridge's, don't allow this to be changed.
newtype LinkMTU = LinkMTU Word32
    deriving (Show, Eq, Num, Ord, Enum, Real, Integral)
instance Message LinkMTU where
    type MessageHeader LinkMTU = IfInfoMsg
    messageAttrs (LinkMTU mtu) =
        AttributeList [word32Attr #{const IFLA_MTU} mtu]
instance Change LinkName LinkMTU where
    changeAttrs n m = messageAttrs n <> messageAttrs m
instance Change LinkIndex LinkMTU where
    changeAttrs _ m = messageAttrs m
instance Reply LinkMTU where
    type ReplyHeader LinkMTU = IfInfoMsg
    fromNLMessage NLMessage{..} = LinkMTU
        <$> findAttributeGet getWord32host [#{const IFLA_MTU}] nlmAttrs

newtype LinkGroup = LinkGroup Word32
    deriving (Show, Eq, Num, Ord, Enum, Real, Integral)
instance Change LinkName LinkGroup where
    changeAttrs n (LinkGroup g) = messageAttrs n <> 
        AttributeList [word32Attr #{const IFLA_GROUP} g]
instance Change LinkIndex LinkGroup where
    changeAttrs _ (LinkGroup g) = AttributeList [word32Attr #{const IFLA_GROUP} g]
instance Reply LinkGroup where
    type ReplyHeader LinkGroup = IfInfoMsg
    fromNLMessage NLMessage {..} = LinkGroup
        <$> findAttributeGet getWord32host [#{const IFLA_GROUP}] nlmAttrs

data LinkStats = LinkStats
    { lsRxPackets         :: Word64 -- ^ Total packets received.
    , lsTxPackets         :: Word64 -- ^ Total packets transmitted.
    , lsRxBytes           :: Word64 -- ^ Total bytes received.
    , lsTxBytes           :: Word64 -- ^ Total bytes transmitted.
    , lsRxErrors          :: Word64 -- ^ Bad packets received.
    , lsTxErrors          :: Word64 -- ^ Packet transmission problems.
    , lsRxDropped         :: Word64 -- ^ Dropped due to full buffers.
    , lsTxDropped         :: Word64 -- ^ Out of memory.
    , lsMulticast         :: Word64 -- ^ Multicast packets received.
    , lsCollisions        :: Word64 -- ^ Packet collisions.
    , lsRxLengthErrors    :: Word64 -- ^ Size/header mismatch.
    , lsRxOverErrors      :: Word64 -- ^ Receive ring-buffer overflow.
    , lsRxCRCErrors       :: Word64 -- ^ CRC errors.
    , lsRxFrameErrors     :: Word64 -- ^ Frame-alignment errors.
    , lsRxFIFOErrors      :: Word64 -- ^ Receiver FIFO overrun.
    , lsRxMissedErrors    :: Word64 -- ^ Receiver missed packets.
    , lsTxAbortedErrors   :: Word64
    , lsTxCarrierErrors   :: Word64
    , lsTxFIFOErrors      :: Word64
    , lsTxHeartbeatErrors :: Word64
    , lsTxWindowErrors    :: Word64
    , lsRxCompressed      :: Word64
    , lsTxCompressed      :: Word64
    , lsRxNoHandler       :: Word64 -- ^ Dropped due to lack of handler.
    } deriving (Show, Eq)
instance Reply LinkStats where
    type ReplyHeader LinkStats = IfInfoMsg
    fromNLMessage NLMessage {..} =
            findAttributeGet (get' getWord64host) [#{const IFLA_STATS64}] nlmAttrs
        <|> findAttributeGet (get' getWord32host) [#{const IFLA_STATS}] nlmAttrs
        where
        get' getter = let g = fromIntegral <$> getter in LinkStats
           <$>g<*>g<*>g<*>g<*>g<*>g<*>g<*>g<*>g<*>g<*>g<*>g
           <*>g<*>g<*>g<*>g<*>g<*>g<*>g<*>g<*>g<*>g<*>g<*>g

-- | The header corresponding to link messages, based on @struct ifinfomsg@
-- from @linux/if_link.h@.
data IfInfoMsg = IfInfoMsg
    { ifIndex  :: Int32  -- ^ The index of the link.
    , ifFlags  :: Word32 -- ^ Operational flags of the link.
    } deriving (Show, Eq)
instance Sized IfInfoMsg where
    size = const #{const sizeof(struct ifinfomsg)}
instance Serialize IfInfoMsg where
    put IfInfoMsg {..} = do
        putWord8      #{const AF_UNSPEC}
        putWord8      0
        putWord16host 0
        putInt32host  ifIndex
        putWord32host ifFlags
        putWord32host 0xffffffff
    get = do
        skip 4
        ifIndex <- getInt32le
        ifFlags <- getWord32host
        _change <- getWord32host
        return $ IfInfoMsg {..}
instance Header IfInfoMsg where
    type HeaderPart IfInfoMsg = IfInfoMsgPart
    fromHeaderParts = toHeader . foldr modify (0,mempty)
        where
        toHeader                   (ix,f) = IfInfoMsg ix $ cfFlags f
        modify (IfInfoMsgIndex ix) (_, f) = (ix, f)
        modify (IfInfoMsgFlags f)  (ix,g) = (ix, f <> g)
    emptyHeader = IfInfoMsg 0 0
instance CreateMessageHeader IfInfoMsg where
    createTypeNumber = const #{const RTM_NEWLINK}
instance DestroyMessageHeader IfInfoMsg where
    destroyTypeNumber = const #{const RTM_DELLINK}
instance ChangeMessageHeader IfInfoMsg where
    changeTypeNumber = const #{const RTM_SETLINK}
instance RequestMessageHeader IfInfoMsg where
    requestTypeNumber = const #{const RTM_GETLINK}
instance ReplyMessageHeader IfInfoMsg where
    replyTypeNumbers = const [#{const RTM_NEWLINK}]

-- | Combinable components of an IfInfoMsg.
data IfInfoMsgPart
    = IfInfoMsgIndex Int32
    | IfInfoMsgFlags (ChangeFlags Word32)
    deriving (Show, Eq)
