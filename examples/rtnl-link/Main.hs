module Main where

import Data.List.Split (splitOn)
import System.Environment
import qualified Data.ByteString.Char8 as S

import System.Linux.RTNetlink
import System.Linux.RTNetlink.Link

type AllLinkInfo = (LinkIndex
                 , (LinkName
                 , (LinkEther
                 , (LinkBroadcastEther
                 , (Maybe LinkType
                 , (Maybe VlanId
                 , (LinkMaster
                 , (LinkMTU
                 , (LinkPromiscuity
                 , (LinkArp
                 , (LinkDebug
                 )))))))))))

usage :: IO ()
usage = do
    prog <- getProgName
    putStrLn $ "Usage: " ++ prog ++ " COMMAND\n"
            ++ "\n"
            ++ "COMMAND\n"
            ++ "\t= create CREATE\n"
            ++ "\t| destroy ID\n"
            ++ "\t| dump [ID [DUMP]]\n"
            ++ "\t| change ID CHANGE\n"
            ++ "\n"
            ++ "ID\n"
            ++ "\t= name <ifname> | index <ifindex>\n"
            ++ "\n"
            ++ "CREATE\n"
            ++ "\t= TYPE <ifname>\n"
            ++ "\n"
            ++ "TYPE\n"
            ++ "\t= bridge | dummy | vlan <idnumber>\n"
            ++ "\n"
            ++ "DUMP\n"
            ++ "\t= state | promiscuity | arp | debug | mtu\n"
            ++ "\n"
            ++ "CHANGE\n"
            ++ "\t= up | down\n"
            ++ "\t| promiscuous | chaste\n"
            ++ "\t| arp | noarp\n"
            ++ "\t| debug | nodebug\n"
            ++ "\t| ether <layer2address>\n"
            ++ "\t| mtu <integer>\n"
            ++ "\t| name <ifname>\n"

main :: IO ()
main = do
    args <- getArgs
    err  <- tryRTNL $ case args of

        "create":"bridge":name':[] -> do
            let name = LinkName $ S.pack name'
            create $ (Bridge, name)

        "create":"dummy":name':[] -> do
            let name = LinkName $ S.pack name'
            create $ (Dummy, name)

        "create":"vlan":ix':idnum':name':[] -> do
            let ix    = LinkIndex $ read ix'
                name  = LinkName $ S.pack name'
                idnum = VlanId $ read idnum'
            create $ (Dot1QVlan ix idnum, name)

        "destroy":"name":name':[] -> do
            let name = LinkName $ S.pack name'
            destroy name

        "destroy":"index":ix':[] -> do
            let ix = LinkIndex $ read ix'
            destroy ix

        "dump":[] -> do
            links <- dump AnyLink
            liftIO $ mapM_ (putStrLn . show) (links::[AllLinkInfo])

        "dump":"index":ix':[] -> do
            let ix = LinkIndex $ read ix'
            states <- dump ix
            liftIO $ mapM_ (putStrLn . show) (states::[AllLinkInfo])

        "dump":"name":name':[] -> do
            let name = LinkName $ S.pack name'
            states <- dump name
            liftIO $ mapM_ (putStrLn . show) (states::[AllLinkInfo])

        "dump":"index":ix':"state":[] -> do
            let ix = LinkIndex $ read ix'
            states <- dump ix
            liftIO $ mapM_ (putStrLn . show) (states::[(LinkIndex,LinkState)])

        "dump":"name":name':"state":[] -> do
            let name = LinkName $ S.pack name'
            states <- dump name
            liftIO $ mapM_ (putStrLn . show) (states::[(LinkName,LinkState)])

        "dump":"index":ix':"promiscuity":[] -> do
            let ix = LinkIndex $ read ix'
            states <- dump ix
            liftIO $ mapM_ (putStrLn . show) (states::[(LinkIndex,LinkPromiscuity)])

        "dump":"name":name':"promiscuity":[] -> do
            let name = LinkName $ S.pack name'
            states <- dump name
            liftIO $ mapM_ (putStrLn . show) (states::[(LinkName,LinkPromiscuity)])

        "dump":"index":ix':"arp":[] -> do
            let ix = LinkIndex $ read ix'
            states <- dump ix
            liftIO $ mapM_ (putStrLn . show) (states::[(LinkIndex,LinkArp)])

        "dump":"name":name':"arp":[] -> do
            let name = LinkName $ S.pack name'
            states <- dump name
            liftIO $ mapM_ (putStrLn . show) (states::[(LinkName,LinkArp)])

        "dump":"index":ix':"mtu":[] -> do
            let ix = LinkIndex $ read ix'
            mtus <- dump ix
            liftIO $ mapM_ (putStrLn . show) (mtus::[(LinkIndex,LinkMTU)])

        "dump":"name":name':"mtu":[] -> do
            let name = LinkName $ S.pack name'
            mtus <- dump name
            liftIO $ mapM_ (putStrLn . show) (mtus::[(LinkName,LinkMTU)])

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

        "change":"name":name':"promiscuous":[] -> do
            let name = LinkName $ S.pack name'
            change name Promiscuous

        "change":"name":name':"chaste":[] -> do
            let name = LinkName $ S.pack name'
            change name Chaste

        "change":"index":ix':"promiscuous":[] -> do
            let ix = LinkIndex $ read ix'
            change ix Promiscuous

        "change":"index":ix':"chaste":[] -> do
            let ix = LinkIndex $ read ix'
            change ix Chaste

        "change":"name":name':"arp":[] -> do
            let name = LinkName $ S.pack name'
            change name Arp

        "change":"name":name':"noarp":[] -> do
            let name = LinkName $ S.pack name'
            change name NoArp

        "change":"index":ix':"arp":[] -> do
            let ix = LinkIndex $ read ix'
            change ix Arp

        "change":"index":ix':"noarp":[] -> do
            let ix = LinkIndex $ read ix'
            change ix NoArp

        "change":"name":name':"debug":[] -> do
            let name = LinkName $ S.pack name'
            change name Debug

        "change":"index":ix':"nodebug":[] -> do
            let ix  = LinkIndex $ read ix'
            change ix NoDebug

        "change":"name":name':"mtu":mtu':[] -> do
            let name = LinkName $ S.pack name'
                mtu  = LinkMTU $ read mtu'
            change name mtu

        "change":"index":ix':"mtu":mtu':[] -> do
            let ix  = LinkIndex $ read ix'
                mtu = LinkMTU $ read mtu'
            change ix mtu

        "change":"name":name':"ether":eth':[] -> do
            let name          = LinkName $ S.pack name'
                [a,b,c,d,e,f] = read . ("0x"++) <$> splitOn ":" eth'
                eth           = LinkEther a b c d e f
            change name eth

        "change":"index":ix':"ether":eth':[] -> do
            let ix            = LinkIndex $ read ix'
                [a,b,c,d,e,f] = read . ("0x"++) <$> splitOn ":" eth'
                eth           = LinkEther a b c d e f
            change ix eth

        "change":"index":ix':"name":name':[] -> do
            let ix   = LinkIndex $ read ix'
                name = LinkName $ S.pack name'
            change ix name

        _ -> liftIO usage

    case err of
        Left  s  -> putStrLn $ "Error: " ++ s
        Right () -> putStrLn $ "Success"
