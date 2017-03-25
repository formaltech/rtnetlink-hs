module Main where

import Data.List.Split (splitOneOf)
import System.Environment
import System.Socket.Family.Inet (inetAddressFromTuple)

import System.Linux.RTNetlink
import System.Linux.RTNetlink.Address

usage :: IO ()
usage = do
    prog <- getProgName
    putStrLn $ "Usage: " ++ prog ++ " COMMAND\n"
            ++ "\n"
            ++ "COMMAND\n"
            ++ "\t= create <ip4>/<mask> index <ifindex>\n"
            ++ "\t| destroy <ip4>/<mask> index <ifindex>\n"
            ++ "\t| dump\n"

main :: IO ()
main = do
    args <- getArgs
    err  <- tryRTNL $ case args of
        "create":ip4':"index":ix':[] -> do
            let ix          = IfIndex $ read ix'
                [a,b,c,d,m] = fmap read . splitOneOf "./" $ ip4'
                address     = inetAddressFromTuple (a,b,c,d)
            create $ IfInetAddress address m ix
        "destroy":ip4':"index":ix':[] -> do
            let ix          = IfIndex $ read ix'
                [a,b,c,d,m] = fmap read . splitOneOf "./" $ ip4'
                address     = inetAddressFromTuple (a,b,c,d)
            destroy $ IfInetAddress address m ix
        "dump":[] -> do
            addresses <- dump AnyInterface
            liftIO $ mapM_ (putStrLn . show) (addresses::[IfInetAddress])
        _ -> liftIO usage
    case err of
        Left  s  -> putStrLn $ "Error: " ++ s
        Right () -> putStrLn $ "Success"
