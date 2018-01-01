module Main where

import Data.List (break)
import Data.List.Split (splitOneOf, splitOn)
import System.Environment
import System.Socket.Family.Inet (inetAddressFromTuple)
import System.Socket.Family.Inet6 (Inet6Address, inet6AddressFromTuple)

import System.Linux.RTNetlink
import System.Linux.RTNetlink.Address

usage :: IO ()
usage = do
    prog <- getProgName
    putStrLn $ "Usage: " ++ prog ++ " COMMAND\n"
            ++ "\n"
            ++ "COMMAND\n"
            ++ "\t= dump DUMP\n"
            ++ "\t| create ipv4 <ipv4>/<mask> index <ifindex>\n"
            ++ "\t| destroy ipv4 <ipv4>/<mask> index <ifindex>\n"
            ++ "\n"
            ++ "DUMP\n"
            ++ "\t= ipv4 | ipv6\n"
            ++ "\n"
            ++ "ADDRESS\n"
            ++ "\t= ipv4 <ipv4>/<mask>\n"
            ++ "\t| ipv6 <u16>:<u16>:<u16>:<u16>:<u16>:<u16>:<u16>:<u16>/<mask>\n"

main :: IO ()
main = do
    args <- getArgs
    err  <- tryRTNL $ case args of

        "dump":"ipv4":[] -> do
            addresses <- dump AnyInterface
            liftIO $ mapM_ (putStrLn . show) (addresses::[IfInetAddress])

        "dump":"ipv6":[] -> do
            addresses <- dump AnyInterface
            liftIO $ mapM_ (putStrLn . show) (addresses::[IfInet6Address])

        "create":"ipv4":ipv4:"index":ix':[] -> do
            let ix          = IfIndex $ read ix'
                [a,b,c,d,m] = read <$> splitOneOf "./" ipv4
                address     = inetAddressFromTuple (a,b,c,d)
                prefix      = IfPrefix m
            create $ IfInetAddress address prefix ix

        "create":"ipv6":ipv6':"index":ix':[] -> do
            let ix                = IfIndex $ read ix'
                (ipv6,'/':m)      = break (=='/') ipv6'
                [a,b,c,d,e,f,g,h] = read . ("0x"++) <$> splitOn ":" ipv6
                address           = inet6AddressFromTuple (a,b,c,d,e,f,g,h)
                prefix            = IfPrefix $ read m
            create $ IfInet6Address address prefix ix

        "destroy":"ipv4":ipv4:"index":ix':[] -> do
            let ix          = IfIndex $ read ix'
                [a,b,c,d,m] = read <$> splitOneOf ":/" ipv4
                address     = inetAddressFromTuple (a,b,c,d)
                prefix      = IfPrefix m
            destroy $ IfInetAddress address prefix ix

        "destroy":"ipv6":ipv6':"index":ix':[] -> do
            let ix                = IfIndex $ read ix'
                (ipv6,'/':m)      = break (=='/') ipv6'
                [a,b,c,d,e,f,g,h] = read . ("0x"++) <$> splitOn ":" ipv6
                address           = inet6AddressFromTuple (a,b,c,d,e,f,g,h)
                prefix            = IfPrefix $ read m
            destroy $ IfInet6Address address prefix ix

        _ -> liftIO usage
    case err of
        Left  s  -> putStrLn $ "Error: " ++ s
        Right () -> putStrLn $ "Success"
