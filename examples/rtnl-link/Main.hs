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
            ++ "\t= TYPE <ifname> BRIDGE_OPTS\n"
            ++ "\n"
            ++ "TYPE\n"
            ++ "\t= bridge\n"

main :: IO ()
main = do
    args <- getArgs
    err  <- tryRTNL $ case args of
        "create":"bridge":name':[] -> do
            let name = LinkName $ S.pack name'
            create $ Bridge name
        "create":"dummy":name':[] -> do
            let name = LinkName $ S.pack name'
            create $ Dummy name
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
