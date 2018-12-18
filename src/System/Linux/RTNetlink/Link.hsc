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
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module System.Linux.RTNetlink.Link where

import Control.Applicative ((<$>), (<*>))
import Data.Bits ((.&.))
import Data.Int (Int32)
import Data.Monoid ((<>))
import Data.Serialize
import Data.String (IsString)
import Data.Word (Word8, Word16, Word32)
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
instance Message LinkIndex where
    type MessageHeader LinkIndex = IfInfoMsg
    messageHeader (LinkIndex ix) = IfInfoMsg (fromIntegral ix) 0
instance Destroy LinkIndex where
    destroyTypeNumber = const #{const RTM_DELLINK}
instance Request LinkIndex where
    requestTypeNumber = const #{const RTM_GETLINK}
instance Reply LinkIndex where
    type ReplyHeader LinkIndex = IfInfoMsg
    replyTypeNumbers = const [#{const RTM_NEWLINK}]
    fromNLMessage    = Just . LinkIndex . fromIntegral . ifIndex . nlmHeader
instance Dump LinkIndex LinkIndex
instance Dump LinkName LinkIndex
instance Dump AnyLink LinkIndex

-- | A link identified by its name.
newtype LinkName = LinkName S.ByteString
    deriving (Show, Eq, IsString)
instance Message LinkName where
    type MessageHeader LinkName = IfInfoMsg
    messageAttrs  (LinkName bs) = AttributeList
        [cStringAttr #{const IFLA_IFNAME} $ S.take #{const IFNAMSIZ} bs]
instance Change LinkIndex LinkName where
    changeTypeNumber _ _ = #{const RTM_SETLINK}
    changeAttrs      n m = messageAttrs n <> messageAttrs m
instance Destroy LinkName where
    destroyTypeNumber = const #{const RTM_DELLINK}
instance Request LinkName where
    requestTypeNumber = const #{const RTM_GETLINK}
instance Reply LinkName where
    type ReplyHeader LinkName = IfInfoMsg
    replyTypeNumbers = const [#{const RTM_NEWLINK}]
    fromNLMessage m  = do
        a <- findAttribute [#{const IFLA_IFNAME}] $ nlmAttrs m
        n <- S.takeWhile (/=0) <$> attributeData a
        return $ LinkName n
instance Dump LinkIndex LinkName
instance Dump LinkName LinkName
instance Dump AnyLink LinkName

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
    messageAttrs e  = AttributeList [Attribute #{const IFLA_ADDRESS} $ encode e]
instance Change LinkName LinkEther where
    changeTypeNumber _ _ = #{const RTM_SETLINK}
    changeAttrs      n m = messageAttrs n <> messageAttrs m
instance Change LinkIndex LinkEther where
    changeTypeNumber _ _ = #{const RTM_SETLINK}
    changeAttrs      n m = messageAttrs n <> messageAttrs m
instance Reply LinkEther where
    type ReplyHeader LinkEther = IfInfoMsg
    replyTypeNumbers = const [#{const RTM_NEWLINK}]
    fromNLMessage m  = do
        a <- findAttribute [#{const IFLA_ADDRESS}] . nlmAttrs $ m
        d <- attributeData a
        decodeMaybe d
instance Dump LinkIndex LinkEther
instance Dump LinkName LinkEther
instance Dump AnyLink LinkEther

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
    messageAttrs e  = AttributeList [Attribute #{const IFLA_BROADCAST} $ encode e]
instance Reply LinkBroadcastEther where
    type ReplyHeader LinkBroadcastEther = IfInfoMsg
    replyTypeNumbers = const [#{const RTM_NEWLINK}]
    fromNLMessage m  = do
        a <- findAttribute [#{const IFLA_BROADCAST}] . nlmAttrs $ m
        d <- attributeData a
        decodeMaybe d
instance Dump LinkIndex LinkBroadcastEther
instance Dump LinkName LinkBroadcastEther
instance Dump AnyLink LinkBroadcastEther

-- | Link wildcard.
data AnyLink = AnyLink
    deriving (Show, Eq)
instance Message AnyLink where
    type MessageHeader AnyLink = IfInfoMsg
instance Request AnyLink where
    requestTypeNumber = const #{const RTM_GETLINK}
    requestNLFlags    = const dumpNLFlags

-- | A dummy interface.
newtype Dummy = Dummy LinkName
    deriving (Show, Eq)
instance Message Dummy where
    type MessageHeader Dummy = IfInfoMsg
    messageAttrs  (Dummy name) = messageAttrs name <> AttributeList
        [ AttributeNest #{const IFLA_LINKINFO}
            [ cStringAttr #{const IFLA_INFO_KIND} "dummy" ]
        ]
instance Create Dummy where
    createTypeNumber = const #{const RTM_NEWLINK}

-- | A bridge interface.
newtype Bridge = Bridge LinkName
    deriving (Show, Eq, IsString)
instance Message Bridge where
    type MessageHeader Bridge = IfInfoMsg
    messageAttrs (Bridge name) = messageAttrs name <> AttributeList
        [ AttributeNest #{const IFLA_LINKINFO}
            [ cStringAttr #{const IFLA_INFO_KIND} "bridge" ]
        ]
instance Create Bridge where
    createTypeNumber = const #{const RTM_NEWLINK}

-- | An 802.1Q vlan interface.
data Dot1QVlan = Dot1QVlan
    LinkIndex -- ^ Index of parent interface
    Word16    -- ^ Vlan ID number
    LinkName  -- ^ Name of new vlan interface
    deriving (Show, Eq)
instance Message Dot1QVlan where
    type MessageHeader Dot1QVlan = IfInfoMsg
    messageAttrs (Dot1QVlan (LinkIndex n) vid name) =
        messageAttrs name <> AttributeList
            [ word32Attr #{const IFLA_LINK} (fromIntegral n)
            , AttributeNest #{const IFLA_LINKINFO}
                [ cStringAttr #{const IFLA_INFO_KIND} "vlan"
                , AttributeNest #{const IFLA_INFO_DATA}
                    [ word16Attr #{const IFLA_VLAN_ID} vid
                    , word16AttrBE #{const IFLA_VLAN_PROTOCOL} #{const ETH_P_8021Q}
                    ]
                ]
            ]
instance Create Dot1QVlan where
    createTypeNumber = const #{const RTM_NEWLINK}

-- | The state of a link.
data LinkState = Up | Down
    deriving (Show, Eq)
instance Reply LinkState where
    type ReplyHeader LinkState = IfInfoMsg
    replyTypeNumbers = const [#{const RTM_NEWLINK}]
    fromNLMessage m  = Just $ if flag == 0 then Down else Up
        where flag = ifFlags (nlmHeader m) .&. #{const IFF_UP}
instance Change LinkName LinkState where
    changeTypeNumber _ _ = #{const RTM_SETLINK}
    changeHeader     n s = IfInfoMsg ix flag
        where
        ix   = ifIndex $ messageHeader n
        flag = if s == Up then #{const IFF_UP} else 0
instance Change LinkIndex LinkState where
    changeTypeNumber _ _ = #{const RTM_SETLINK}
    changeHeader     n s = IfInfoMsg ix flag
        where
        ix   = ifIndex $ messageHeader n
        flag = if s == Up then #{const IFF_UP} else 0
instance Dump LinkIndex LinkState
instance Dump LinkName LinkState
instance Dump AnyLink LinkState

-- | A 'Promiscuous' link accepts all frames at layer 2; a 'Chaste' one accepts
-- just those addressed to it and possibly ones sent to the broadcast address.
data LinkPromiscuity = Promiscuous | Chaste
    deriving (Show, Eq)
instance Reply LinkPromiscuity where
    type ReplyHeader LinkPromiscuity = IfInfoMsg
    replyTypeNumbers = const [#{const RTM_NEWLINK}]
    fromNLMessage m  = Just $ if flag == 0 then Chaste else Promiscuous
        where flag = ifFlags (nlmHeader m) .&. #{const IFF_PROMISC}
instance Change LinkName LinkPromiscuity where
    changeTypeNumber _ _ = #{const RTM_SETLINK}
    changeHeader     n s = IfInfoMsg ix flag
        where
        ix   = ifIndex $ messageHeader n
        flag = if s == Promiscuous then #{const IFF_PROMISC} else 0
instance Change LinkIndex LinkPromiscuity where
    changeTypeNumber _ _ = #{const RTM_SETLINK}
    changeHeader     n s = IfInfoMsg ix flag
        where
        ix   = ifIndex $ messageHeader n
        flag = if s == Promiscuous then #{const IFF_PROMISC} else 0
instance Dump LinkIndex LinkPromiscuity
instance Dump LinkName LinkPromiscuity
instance Dump AnyLink LinkPromiscuity

-- | Whether to use ARP on the interface to resolve L3 addresses to L2 ones.
data LinkArp = Arp | NoArp
    deriving (Show, Eq)
instance Reply LinkArp where
    type ReplyHeader LinkArp = IfInfoMsg
    replyTypeNumbers = const [#{const RTM_NEWLINK}]
    fromNLMessage m  = Just $ if flag == 0 then Arp else NoArp
        where flag = ifFlags (nlmHeader m) .&. #{const IFF_NOARP}
instance Change LinkName LinkArp where
    changeTypeNumber _ _ = #{const RTM_SETLINK}
    changeHeader     n s = IfInfoMsg ix flag
        where
        ix   = ifIndex $ messageHeader n
        flag = if s == NoArp then #{const IFF_NOARP} else 0
instance Change LinkIndex LinkArp where
    changeTypeNumber _ _ = #{const RTM_SETLINK}
    changeHeader     n s = IfInfoMsg ix flag
        where
        ix   = ifIndex $ messageHeader n
        flag = if s == NoArp then #{const IFF_NOARP} else 0
instance Dump LinkIndex LinkArp
instance Dump LinkName LinkArp
instance Dump AnyLink LinkArp

-- | Internal debug flag. If this is supported by the driver, it will generally
-- spew some extra information into @dmesg@.
data LinkDebug = Debug | NoDebug
    deriving (Show, Eq)
instance Reply LinkDebug where
    type ReplyHeader LinkDebug = IfInfoMsg
    replyTypeNumbers = const [#{const RTM_NEWLINK}]
    fromNLMessage m  = Just $ if flag == 0 then NoDebug else Debug
        where flag = ifFlags (nlmHeader m) .&. #{const IFF_DEBUG}
instance Change LinkName LinkDebug where
    changeTypeNumber _ _ = #{const RTM_SETLINK}
    changeHeader     n s = IfInfoMsg ix flag
        where
        ix   = ifIndex $ messageHeader n
        flag = if s == Debug then #{const IFF_DEBUG} else 0
instance Change LinkIndex LinkDebug where
    changeTypeNumber _ _ = #{const RTM_SETLINK}
    changeHeader     n s = IfInfoMsg ix flag
        where
        ix   = ifIndex $ messageHeader n
        flag = if s == Debug then #{const IFF_DEBUG} else 0
instance Dump LinkIndex LinkDebug
instance Dump LinkName LinkDebug
instance Dump AnyLink LinkDebug

-- | Maximum transmission unit for a link. Note that some interface types, such
-- as 'Bridge's, don't allow this to be changed.
newtype LinkMTU = LinkMTU Word32
    deriving (Show, Eq, Num, Ord, Enum, Real, Integral)
instance Message LinkMTU where
    type MessageHeader LinkMTU = IfInfoMsg
    messageAttrs (LinkMTU mtu) =
        AttributeList [word32Attr #{const IFLA_MTU} mtu]
instance Change LinkName LinkMTU where
    changeTypeNumber _ _ = #{const RTM_SETLINK}
    changeAttrs      n m = messageAttrs n <> messageAttrs m
instance Change LinkIndex LinkMTU where
    changeTypeNumber _ _ = #{const RTM_SETLINK}
    changeAttrs      n m = messageAttrs n <> messageAttrs m
instance Reply LinkMTU where
    type ReplyHeader LinkMTU = IfInfoMsg
    replyTypeNumbers = const [#{const RTM_NEWLINK}]
    fromNLMessage m  = do
        a <- findAttribute [#{const IFLA_MTU}] $ nlmAttrs m
        d <- attributeData a
        LinkMTU <$> runGetMaybe getWord32host d
instance Dump LinkIndex LinkMTU
instance Dump LinkName LinkMTU
instance Dump AnyLink LinkMTU

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
    emptyHeader = IfInfoMsg 0 0
