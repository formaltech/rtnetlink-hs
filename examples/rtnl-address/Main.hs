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
            ++ "\t= create ipv4 <ipv4>/<mask> index <ifindex>\n"
            ++ "\t| destroy ipv4 <ipv4>/<mask> index <ifindex>\n"
            ++ "\t| dump ipv4\n"
            ++ "\t| dump ipv6\n"

main :: IO ()
main = do
    args <- getArgs
    err  <- tryRTNL $ case args of
        "create":"ipv4":ipv4:"index":ix':[] -> do
            let ix          = IfIndex $ read ix'
                [a,b,c,d,m] = fmap read . splitOneOf "./" $ ipv4
                address     = inetAddressFromTuple (a,b,c,d)
            create $ IfInetAddress address m ix
        "destroy":"ipv4":ipv4:"index":ix':[] -> do
            let ix          = IfIndex $ read ix'
                [a,b,c,d,m] = fmap read . splitOneOf "./" $ ipv4
                address     = inetAddressFromTuple (a,b,c,d)
            destroy $ IfInetAddress address m ix
        "dump":"ipv4":[] -> do
            addresses <- dump AnyInterface
            liftIO $ mapM_ (putStrLn . show) (addresses::[IfInetAddress])
        "dump":"ipv6":[] -> do
            addresses <- dump AnyInterface
            liftIO $ mapM_ (putStrLn . show) (addresses::[IfInet6Address])
        _ -> liftIO usage
    case err of
        Left  s  -> putStrLn $ "Error: " ++ s
        Right () -> putStrLn $ "Success"
