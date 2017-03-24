{-|
Module      : System.Socket.Family.Netlink
Description : Extends System.Socket with the netlink socket family.
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
{-# LANGUAGE TypeFamilies #-}
module System.Socket.Family.Netlink
    ( Netlink
    , SocketAddress()
    , NetlinkGroup(..)
    , netlinkAddress
    , netlinkAddressPid
    , netlinkKernel
    ) where

import Data.Bits ((.|.))
import Data.Serialize (Serialize(..), encode, decode)
import Data.Serialize (putWord16host, putWord32host, getWord16host, getWord32host)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import GHC.Word (Word32)
import System.Posix (getProcessID)
import qualified Data.ByteString.Char8 as S

import System.Socket

#include <linux/netlink.h>
#include <sys/socket.h>

-- | Netlink socket family.
data Netlink
instance Family Netlink where
    familyNumber _ = #{const AF_NETLINK}

-- | Netlink address corresponding to @struct sockaddr_nl@ from @linux/netlink.h@.
data instance SocketAddress Netlink = SocketAddressNetlink
    { netlinkPid    :: Word32 -- ^ Netlink source address.
    , netlinkGroups :: Word32 -- ^ Group subscription mask.
    } deriving (Read, Show, Eq)
instance Serialize (SocketAddress Netlink) where
    put nl = do
        putWord16host $ #{const AF_NETLINK}
        putWord16host $ 0
        putWord32host $ netlinkPid nl
        putWord32host $ netlinkGroups nl
        putWord32host $ 0
    get = do
        _nl_family <- getWord16host
        _nl_pad16  <- getWord16host
        nl_pid     <- getWord32host
        nl_groups  <- getWord32host
        _nl_pad32  <- getWord32host
        return $ SocketAddressNetlink nl_pid nl_groups
instance Storable (SocketAddress Netlink) where
    sizeOf    _ = #{const sizeof(struct sockaddr_nl)}
    alignment _ = 4
    peek ptr    = do
        bs <- S.pack <$> mapM (peekByteOff ptr) [0..15]
        case decode bs of
            Left e   -> fail e
            Right nl -> return nl
    poke ptr nl =
        let pokePtr = pokeByteOff $ castPtr ptr
         in mapM_ (uncurry pokePtr) $ [0..15] `zip` S.unpack (encode nl)

-- | Class of netlink groups. This is extensible because groups vary by netlink
-- subsystem.
class NetlinkGroup g where
    netlinkGroupNumber :: g -> Word32

-- | Construct a group mask from a list of groups.
netlinkGroupMask :: NetlinkGroup g => [g] -> Word32
netlinkGroupMask = foldr (.|.) 0 . fmap netlinkGroupNumber

-- | Construct a netlink socket from a collection of groups.
netlinkAddress :: NetlinkGroup g => [g] -> IO (SocketAddress Netlink)
netlinkAddress gs = do
    pid <- fromIntegral <$> getProcessID
    return $ SocketAddressNetlink pid (netlinkGroupMask gs)

-- | Like 'netlinkAddress', but with a configurable source address.
netlinkAddressPid :: NetlinkGroup g => Word32 -> [g] -> SocketAddress Netlink
netlinkAddressPid pid = SocketAddressNetlink pid . netlinkGroupMask

-- | The kernel's address.
netlinkKernel :: SocketAddress Netlink
netlinkKernel = SocketAddressNetlink 0 0
