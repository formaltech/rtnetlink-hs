{-|
Module      : System.Linux.RTNetlink
Description : Basic high-level tools for speaking RTNetlink with the Linux
              kernel.
Copyright   : (c) Formaltech Inc. 2017
License     : BSD3
Maintainer  : protob3n@gmail.com
Stability   : experimental
Portability : Linux

RTNetlink is an extensible, high-level, pure Haskell interface for manipulating
network interfaces on Linux: creating and destroying interfaces, changing and
dumping interface settings, adding and removing addresses.

The core interface of RTNetlink is the 'RTNL' monad. 'RTNL' handles the heavy
lifting of opening and closing netlink sockets, incrementing sequence numbers,
and getting the responses for the current sequence number behind the scenes.
Messages not that are not responses to a sent message, such as those sent to
group subscribers, are stored in the backlog and can be retrieved with
'getBacklog'.

The basic way to use 'RTNL' is to use the 'create', 'destroy', 'dump', and
'change' convenience functions. If you want more control, you can use 'talk'
and 'talk_'. Import modules like "System.Linux.RTNetlink.Link" to get access
to prefab instances of 'Create' and 'Destroy' messages, etc. Or import
"System.Linux.RTNetlink.Message" to get access to the core typeclasses and
create your own messages. "System.Linux.RTNetlink.Packet" has a number of
functions to make this easier.

= Example:

>   {-# LANGUAGE OverloadedStrings #-}
>   module Main where
>
>   import System.Linux.RTNetlink
>   import System.Linux.RTNetlink.Link
>   import Control.Monad (when)
>   
>   main :: IO ()
>   main = runRTNL $ do
>       let mybridge = LinkName "mybridge"
>       create (Bridge mybridge)
>       change mybridge Up
>       state <- dump mybridge
>       when (head state == Up) $
>           liftIO (putStrLn "I did it, mom!")
>       destroy mybridge
-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Linux.RTNetlink (
    -- * The RTNL monad
      RTNL()
    , tryRTNL
    , runRTNL
    , runRTNLGroups
    -- * High-level communication
    , create
    , destroy
    , dump
    , change
    , getBacklog
    , clearBacklog
    -- * Lower-level communication
    , talk
    , talk_
    , talkRaw
    , toggleVerbose
    -- * Utility functions
    , liftIO
    ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Monad.State (get, gets, put, modify, modify')
import Data.Monoid (mempty)
import Data.Either (partitionEithers)
import Data.List (partition)
import Data.Serialize (encode)
import Foreign.C.Error (Errno(..), eOK, errnoToIOError)
import Hexdump (prettyHex)
import System.Random (randomIO)
import System.Socket (Socket, MessageFlags, SocketException(..))
import System.Socket (socket, bind, send, receive, close)
import System.Socket.Type.Raw (Raw)
import System.Timeout (timeout)
import qualified Control.Exception as X
import qualified Data.ByteString as S

import System.Linux.RTNetlink.Message
import System.Linux.RTNetlink.Packet
import System.Linux.RTNetlink.Util
import System.Socket.Family.Netlink
import System.Socket.Protocol.RTNetlink

data Handle = Handle
    { _handle :: Socket Netlink Raw RTNetlink
    , backlog :: [S.ByteString]
    , verbose :: Bool
    , seqNum  :: SequenceNumber
    }

-- | RTNL monad to simplify netlink communication.
newtype RTNL a = RTNL {unRTNL :: StateT Handle IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadState Handle)

-- | Run an RTNL function and catch all @IOError@s. This means that functions
-- in this module are guaranteed not to throw uncaught exceptions.
tryRTNL :: RTNL a -> IO (Either String a)
tryRTNL = fmap (left (\e -> show (e::IOError))) . X.try . runRTNL

-- | Run an RTNL function. RTNL functions in this module throw exclusively
-- @IOError@s.
runRTNL :: RTNL a -> IO a
runRTNL = runRTNLGroups []

-- | Run an RTNL function and specify some groups to subscribe to.
runRTNLGroups :: [RTNetlinkGroup] -> RTNL a -> IO a
runRTNLGroups gs r = X.bracket (rethrow "socket" socket) close $ \s -> do
    rethrow "bind" $ bind s =<< netlinkAddress gs
    h <- Handle s [] False <$> randomIO
    evalStateT (unRTNL r) h

-- | Lowest-level RTNL function. Send a @BytsString@ and receive all responses
-- and queued messages as @ByteString@s.
--
-- _Note:_ This function does nothing to manage sequence numbers or distinguish
-- between responses and queued messages. Nothing will be added to the backlog.
talkRaw :: S.ByteString -> RTNL [S.ByteString]
talkRaw packet = do
    Handle h b v n <- RTNL get
    when v $ liftIO . putStrLn $ "SEND:\n" ++ prettyHex packet
    _   <- liftIO . rethrow "send" $ send h packet mempty
    bss <- getResponses
    when v $ liftIO . flip mapM_ bss $ \bs -> putStrLn ("RECV:\n" ++ prettyHex bs)
    let (rs',ms) = partition ((==n) . sequenceNumber) bss
    RTNL . put $ Handle h (ms++b) v n
    return rs'

-- | Send any 'NLMessage' and receive a list of 'Reply's.
--
-- If the 'ReplyTypeNumbers' of the return type do not include NLM_ERROR, any
-- non-zero error messages received will be thrown as @IOError@s. Responses
-- that don't parse as the return type will be ignored.
talk :: (Header h, Reply r) => (SequenceNumber -> NLMessage h) -> RTNL [r]
talk m = do
    n   <- RTNL $ gets seqNum
    bss <- talkRaw . encode $ m n
    RTNL . modify $ \h -> h {seqNum = n + 1}
    let (bss',rs) = partitionEithers $ fmap tryDecodeReply bss
        (_,es)    = partitionEithers $ fmap tryDecodeReply bss'
    case filter (/=eOK) es of
        e:_ -> liftIO . X.throwIO $ errnoToIOError "RTNETLINK answers" e Nothing Nothing
        _   -> return rs

-- | Like 'talk', but discards non-error 'Reply's.
talk_ :: Header h => (SequenceNumber -> NLMessage h) -> RTNL ()
talk_ m = void (talk m :: RTNL [()])

-- | Send a 'Create' message and ignore non-error 'Reply's.
create :: Create c => c -> RTNL ()
create = talk_ . createNLMessage

-- | Send a 'Destroy' message and ignore non-error 'Reply's.
destroy :: Destroy d => d -> RTNL ()
destroy = talk_ . destroyNLMessage

-- | Send a 'Request' and receive the associated 'Reply's.
dump :: Dump q r => q -> RTNL [r]
dump = talk . requestNLMessage

-- | Send a 'Change' message and ignore non-error 'Reply's.
change :: Change id c => id -> c -> RTNL ()
change i c = talk_ $ changeNLMessage i c

-- | Get all the 'Reply's of a particular type in the backlog and queued
-- on the socket.
getBacklog :: Reply r => RTNL [r]
getBacklog = do
    b  <- RTNL $ gets backlog
    ms <- getResponses
    let (b',rs) = partitionEithers $ fmap tryDecodeReply (ms++b)
    RTNL . modify' $ \h -> h {backlog = b'}
    return rs

-- | Clear the backlog.
clearBacklog :: RTNL ()
clearBacklog = RTNL . modify' $ \h -> h {backlog = []}

toggleVerbose :: RTNL ()
toggleVerbose = RTNL . modify $ \h -> h {verbose = not $ verbose h}

-- Internal

-- | Return all the responses for the current sequence number.
getResponses :: RTNL [S.ByteString]
getResponses = do
    Handle h b v n <- RTNL get
    ps           <- liftIO $ receiveAll h 8192 mempty
    let ms       = concatMap splitMessages ps
        (rs,ms') = partition ((==n) . sequenceNumber) ms
    RTNL . put $ Handle h (b ++ ms') v n
    return rs

-- | Try to decode a 'Reply'. If that fails, send the original 'S.ByteString'
-- back.
tryDecodeReply :: Reply r => S.ByteString -> Either S.ByteString r
tryDecodeReply bs = maybe (Left bs) Right $ fromNLMessage' =<< decodeMaybe bs

-- Util

-- | Receive all packets queued on the socket without blocking.
receiveAll :: Socket f t p -> Int -> MessageFlags -> IO [S.ByteString]
receiveAll s n f = unfoldM . timeout 500 . rethrow "receive" $ receive s n f

-- | Re-throw a SocketException as an IOError.
rethrow :: String -> IO a -> IO a
rethrow name = X.handle $ \(SocketException n) ->
    X.throwIO $ errnoToIOError name (Errno n) Nothing Nothing
