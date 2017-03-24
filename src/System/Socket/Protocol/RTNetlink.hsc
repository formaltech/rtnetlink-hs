{-|
Module      : System.Socket.Protocol.RTNetlink
Description : Extends System.Socket with the ROUTE_NETLINK socket protocol.
Copyright   : (c) Formaltech Inc. 2017
License     : BSD3
Maintainer  : protob3n@gmail.com
Stability   : experimental
Portability : Linux
-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module System.Socket.Protocol.RTNetlink
    ( RTNetlink
    , RTNetlinkGroup(..)
    ) where

import Data.Bits (shift)

import System.Socket (Protocol(..))
import System.Socket.Family.Netlink (NetlinkGroup(..))

#include <linux/rtnetlink.h>

-- | Protocol for the ROUTE_NETLINK subsystem of netlink.
data RTNetlink
instance Protocol RTNetlink where
    protocolNumber _ = 0

-- | Multicast groups the user of an RTNetlink socket can subscribe to.
data RTNetlinkGroup
    = RTNetlinkGroupNone
    | RTNetlinkGroupLink
    | RTNetlinkGroupNotify
    | RTNetlinkGroupNeighbor
    | RTNetlinkGroupTC
    | RTNetlinkGroupIPv4IfAddr
    | RTNetlinkGroupIPv4MRoute
    | RTNetlinkGroupIPv4Route
    | RTNetlinkGroupIPv4Rule
    | RTNetlinkGroupIPv6IfAddr
    | RTNetlinkGroupIPv6MRoute
    | RTNetlinkGroupIPv6Route
    | RTNetlinkGroupIPv6IfInfo
    | RTNetlinkGroupDECnetIfAddr
    | RTNetlinkGroupDECnetRoute
    | RTNetlinkGroupDECnetRule
    | RTNetlinkGroupIPv6Prefix
    | RTNetlinkGroupIPv6Rule
    | RTNetlinkGroupNDUserOpt
    | RTNetlinkGroupPhonetIfAddr
    | RTNetlinkGroupPhonetRoute
    | RTNetlinkGroupDCB
    | RTNetlinkGroupIPv4Netconf
    | RTNetlinkGroupIPv6Netconf
    | RTNetlinkGroupMDB
    | RTNetlinkGroupMPLSRoute
    | RTNetlinkGroupNSID
    deriving (Read, Show, Eq)
instance NetlinkGroup RTNetlinkGroup where
    netlinkGroupNumber g = shift 1 $ bit g - 1

bit :: RTNetlinkGroup -> Int
bit RTNetlinkGroupNone         = #const RTNLGRP_NONE
bit RTNetlinkGroupLink         = #const RTNLGRP_LINK
bit RTNetlinkGroupNotify       = #const RTNLGRP_NOTIFY
bit RTNetlinkGroupNeighbor     = #const RTNLGRP_NEIGH
bit RTNetlinkGroupTC           = #const RTNLGRP_TC
bit RTNetlinkGroupIPv4IfAddr   = #const RTNLGRP_IPV4_IFADDR
bit RTNetlinkGroupIPv4MRoute   = #const RTNLGRP_IPV4_MROUTE
bit RTNetlinkGroupIPv4Route    = #const RTNLGRP_IPV4_ROUTE
bit RTNetlinkGroupIPv4Rule     = #const RTNLGRP_IPV4_RULE
bit RTNetlinkGroupIPv6IfAddr   = #const RTNLGRP_IPV6_IFADDR
bit RTNetlinkGroupIPv6MRoute   = #const RTNLGRP_IPV6_MROUTE
bit RTNetlinkGroupIPv6Route    = #const RTNLGRP_IPV6_ROUTE
bit RTNetlinkGroupIPv6IfInfo   = #const RTNLGRP_IPV6_IFINFO
bit RTNetlinkGroupDECnetIfAddr = #const RTNLGRP_DECnet_IFADDR
bit RTNetlinkGroupDECnetRoute  = #const RTNLGRP_DECnet_ROUTE
bit RTNetlinkGroupDECnetRule   = #const RTNLGRP_DECnet_RULE
bit RTNetlinkGroupIPv6Prefix   = #const RTNLGRP_IPV6_PREFIX
bit RTNetlinkGroupIPv6Rule     = #const RTNLGRP_IPV6_RULE
bit RTNetlinkGroupNDUserOpt    = #const RTNLGRP_ND_USEROPT
bit RTNetlinkGroupPhonetIfAddr = #const RTNLGRP_PHONET_IFADDR
bit RTNetlinkGroupPhonetRoute  = #const RTNLGRP_PHONET_ROUTE
bit RTNetlinkGroupDCB          = #const RTNLGRP_DCB
bit RTNetlinkGroupIPv4Netconf  = #const RTNLGRP_IPV4_NETCONF
bit RTNetlinkGroupIPv6Netconf  = #const RTNLGRP_IPV6_NETCONF
bit RTNetlinkGroupMDB          = #const RTNLGRP_MDB
bit RTNetlinkGroupMPLSRoute    = #const RTNLGRP_MPLS_ROUTE
bit RTNetlinkGroupNSID         = #const RTNLGRP_NSID
