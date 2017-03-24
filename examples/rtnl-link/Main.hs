module Main where

import System.Environment
import qualified Data.ByteString.Char8 as S

import System.Linux.RTNetlink
import System.Linux.RTNetlink.Link

usage :: IO ()
usage = do
    prog <- getProgName
    putStrLn $ "Usage: " ++ prog ++ " COMMAND\n"
            ++ "\n"
            ++ "COMMAND\n"
            ++ "\t= create CREATE\n"
            ++ "\t| destroy (name <ifname> | index <ifindex>)\n"
            ++ "\t| dump [(name <ifname> | index <ifindex>) state]\n"
            ++ "\t| change (name <ifname> | index <ifindex>) (up | down)\n"
            ++ "\n"
            ++ "CREATE\n"
            ++ "\t= bridge <ifname> BRIDGE_OPTS\n"
            ++ "\n"
            ++ "BRIDGE_OPTS\n"
            ++ "\t= forward_delay <number>\n"
            ++ "\t| hello_time <number>\n"
            ++ "\t| max_age <number>\n"
            ++ "\t| ageing_time <number>\n"
            ++ "\t| stp_state <number>\n"
            ++ "\t| priority <number>\n"

bridgeOpts :: String -> [String] -> Bridge
bridgeOpts name' opts' = Bridge name delay hello max_age ageing stp priority
    where
    name             = LinkName $ S.pack name'
    delay            = read <$> lookup "forward_delay" opts
    hello            = read <$> lookup "hello_time" opts
    max_age          = read <$> lookup "max_age" opts
    ageing           = read <$> lookup "ageing_time" opts
    stp              = read <$> lookup "stp_state" opts
    priority         = read <$> lookup "priority" opts
    opts             = popts [] opts'
    popts a []       = a
    popts _ (_:[])   = error "Bridge option without a matching value"
    popts a (p:q:os) = popts ((p,q):a) os

main :: IO ()
main = do
    args <- getArgs
    err  <- tryRTNL $ case args of
        "create":"bridge":name:opts -> do
            create $ bridgeOpts name opts
        "destroy":"name":name':[] -> do
            let name = LinkName $ S.pack name'
            destroy name
        "destroy":"index":ix':[] -> do
            let ix = LinkIndex $ read ix'
            destroy ix
        "dump":[] -> do
            names <- dump AnyLink
            liftIO $ mapM_ (putStrLn . show) (names::[(LinkIndex,(LinkName,LinkEther))])
        "dump":"index":ix':"state":[] -> do
            let ix = LinkIndex $ read ix'
            states <- dump ix
            liftIO $ mapM_ (putStrLn . show) (states::[(LinkIndex,LinkState)])
        "dump":"name":name':"state":[] -> do
            let name = LinkName $ S.pack name'
            states <- dump name
            liftIO $ mapM_ (putStrLn . show) (states::[(LinkName,LinkState)])
        "change":"name":name':"up":[] -> do
            let name = LinkName $ S.pack name'
            change name Up
        "change":"name":name':"down":[] -> do
            let name = LinkName $ S.pack name'
            change name Down
        "change":"index":ix':"up":[] -> do
            let ix = LinkIndex $ read ix'
            change ix Up
        "change":"index":ix':"down":[] -> do
            let ix = LinkIndex $ read ix'
            change ix Down
        _ -> liftIO usage
    case err of
        Left  s  -> putStrLn $ "Error: " ++ s
        Right () -> putStrLn $ "Success"
