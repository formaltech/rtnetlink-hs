module Main where

import Data.List.Split (splitOneOf)
import System.Environment
import System.Socket.Family.Inet (inetAddressFromTuple)

import System.Linux.RTNetlink
import System.Linux.RTNetlink.Route
import System.Linux.RTNetlink.Scope

usage :: IO ()
usage = do
    prog <- getProgName
    putStrLn $ "Usage: " ++ prog ++ " COMMAND\n"
            ++ "\n"
            ++ "COMMAND\n"
            ++ "\t= create ipv4 <ipv4> index <ifindex>\n"
            ++ "\t| destroy ipv4 <ipv4> index <ifindex>\n"
            ++ "\t| dump ipv4\n"
            ++ "\t| dump ipv6\n"

simpleRoute addr oif =
  RouteInfo {
      routeDstLen = RouteDstLen 32
    , routeSrcLen = RouteSrcLen 0
    , routeTOS = RouteTOS 0
    , routeOIFIndex = RouteOIFIndex oif
    , routeTable = RouteTableLocal
    , routeScope = ScopeHost
    , routeType = RouteTypeLocal
    , routeDstAddress = Just $ RouteAddress addr
    , routeGateway = Nothing
    }

main :: IO ()
main = do
    args <- getArgs
    err  <- tryRTNL $ case args of
        "create":"ipv4":ipv4:"index":ix':[] -> do
            let
                [a,b,c,d] = fmap read . splitOneOf "." $ ipv4
                address     = inetAddressFromTuple (a,b,c,d)

            create $ simpleRoute address (read ix')
        "destroy":"ipv4":ipv4:"index":ix':[] -> do
            let [a,b,c,d] = fmap read . splitOneOf "." $ ipv4
                address     = inetAddressFromTuple (a,b,c,d)

            destroy $ simpleRoute address (read ix')
        "dump":"ipv4":[] -> do
            routes <- dump AnyRoute
            liftIO $ mapM_ (putStrLn . show) (routes::[RouteInfo])
        "dump":"ipv6":[] -> do
            routes <- dump AnyRoute
            liftIO $ mapM_ (putStrLn . show) (routes::[RouteInfo])
        _ -> liftIO usage
    case err of
        Left  s  -> putStrLn $ "Error: " ++ s
        Right () -> putStrLn $ "Success"
