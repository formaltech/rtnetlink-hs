{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Catch (throwM, bracket)
import Data.Functor ((<$>))
import Data.List ((\\))
import System.Posix (getEffectiveUserID)
import System.Socket.Family.Inet (inetAddressFromTuple)
import System.Socket.Family.Inet6 (inet6AddressFromTuple)
import System.Linux.Namespaces (Namespace(..), UserMapping(..))
import System.Linux.Namespaces (writeUserMappings, unshare)
import qualified Data.ByteString.Char8 as S

import Test.Hspec

import System.Linux.RTNetlink
import System.Linux.RTNetlink.Link
import System.Linux.RTNetlink.Address

testLink :: LinkName
testLink = "test-link"

testLinkEther :: LinkEther
testLinkEther = LinkEther 0xaa 0xbb 0xcc 0xdd 0xee 0xff

testLinkBroadcastEther :: LinkBroadcastEther
testLinkBroadcastEther = LinkBroadcastEther 0xff 0xff 0xff 0x00 0x00 0x00

testLinkIndex :: LinkIndex
testLinkIndex = 9001

testVlan :: LinkName
testVlan = "test-vlan"

testVlanId :: VlanId
testVlanId = 101

testVlanIndex :: LinkIndex
testVlanIndex = 9002

testBridge :: LinkName
testBridge = "test-bridge"

testBridgeIndex :: LinkIndex
testBridgeIndex = 9003

notALink :: LinkName
notALink = "not-a-link"

testAddress4 :: InetAddress
testAddress4 = inetAddressFromTuple (169,254,42,42)

testIfPrefix4 :: IfPrefix
testIfPrefix4 = 17

testIfInetAddress :: IfIndex -> IfInetAddress
testIfInetAddress = IfInetAddress testAddress4 testIfPrefix4

testAddress6 :: Inet6Address
testAddress6 = inet6AddressFromTuple (0xfe80,42,42,42,42,42,42,42)

testIfPrefix6 :: IfPrefix
testIfPrefix6 = 64

testIfInet6Address :: IfIndex -> IfInet6Address
testIfInet6Address = IfInet6Address testAddress6 testIfPrefix6

testIfIndex :: IfIndex
testIfIndex = fromIntegral testLinkIndex

withTestInterface_ :: (IfIndex -> RTNL a) -> RTNL a
withTestInterface_ m = withTestLink_ $ \(LinkIndex n) -> do
    let ix = IfIndex n
    create $ testIfInetAddress ix
    create $ testIfInet6Address ix
    m ix

withTestInterface :: RTNL a -> RTNL a
withTestInterface = withTestInterface_ . const

createTestLink :: RTNL LinkIndex
createTestLink = testLinkIndex <$
    create (testLink
         , (testLinkEther
         , (testLinkBroadcastEther
         , (testLinkIndex
         , (Dummy
         )))))

withTestLink_ :: (LinkIndex -> RTNL a) -> RTNL a
withTestLink_ = bracket createTestLink destroy

withTestLink :: RTNL a -> RTNL a
withTestLink = withTestLink_ . const

withTestVlan_ :: (LinkIndex -> RTNL a) -> RTNL a
withTestVlan_ m = withTestLink_ $ \ix -> do
    create ((Dot1QVlan ix testVlanId, testVlan), testVlanIndex)
    m testVlanIndex

withTestVlan :: RTNL a -> RTNL a
withTestVlan = withTestVlan_ . const

createTestBridge :: RTNL LinkIndex
createTestBridge = testBridgeIndex <$
    create ((Bridge, testBridge), testBridgeIndex)

withTestBridge_ :: (LinkIndex -> RTNL a) -> RTNL a
withTestBridge_ = bracket createTestBridge destroy

withTestBridge :: RTNL a -> RTNL a
withTestBridge = withTestBridge_ . const

rtnlShouldReturn :: (HasCallStack, Show a, Eq a) => RTNL a -> a -> RTNL ()
rtnlShouldReturn m a = do
    a' <- m
    liftIO $ a `shouldBe` a'

main :: IO ()
main = do
    euid <- getEffectiveUserID
    unshare [User, Network]
    writeUserMappings Nothing [UserMapping 0 euid 1]
    hspec $ do
        describe "dump" testDump
        describe "create" testCreate
        describe "change" testChange
        describe "destroy" testDestroy

testDump :: Spec
testDump = do
    context "when operating on layer-2 links" $ do
        it "gets link names" . runRTNL . withTestLink $
            dump' testLinkIndex `rtnlShouldReturn` testLink

        it "gets link indices" . runRTNL . withTestLink $
            dump' testLinkIndex `rtnlShouldReturn` testLinkIndex

        it "gets link ethernet addresses" . runRTNL . withTestLink $
            dump' testLinkIndex `rtnlShouldReturn` testLinkEther

        it "gets link broadcast ethernets" . runRTNL . withTestLink $
            dump' testLinkIndex `rtnlShouldReturn` testLinkBroadcastEther

        it "gets link states" . runRTNL . withTestLink $
            dump' testLinkIndex `rtnlShouldReturn` Down

        it "gets link promiscuity" . runRTNL . withTestLink $  
            dump' testLinkIndex `rtnlShouldReturn` Chaste

        it "gets link arp state" . runRTNL . withTestLink $ 
            dump' testLinkIndex `rtnlShouldReturn` Arp

        it "gets link debug state" . runRTNL . withTestLink $ 
            dump' testLinkIndex `rtnlShouldReturn` NoDebug

        it "gets link MTUs" . runRTNL . withTestLink $ 
            dump' testLinkIndex `rtnlShouldReturn` LinkMTU 1500

        it "gets link stats" . runRTNL . withTestLink $ 
            dump testLinkIndex `rtnlShouldReturn`
                [LinkStats 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]

        it "gets link groups" . runRTNL . withTestLink $ 
            dump' testLinkIndex `rtnlShouldReturn` LinkGroup 0

        it "gets link types" . runRTNL . withTestLink $
            dump' testLinkIndex `rtnlShouldReturn` Dummy

        it "gets link vlan ids" . runRTNL . withTestVlan $
            dump' testVlanIndex `rtnlShouldReturn` testVlanId

        it "gets link masters" . runRTNL . withTestLink . withTestBridge $ do
            change testLinkIndex $ Master testBridgeIndex
            dump' testLinkIndex `rtnlShouldReturn` Master testBridgeIndex

        context "when given a non-existent link name" $ do
            it "throws an exception" $ do
                runRTNL (dump notALink :: RTNL [()])
                    `shouldThrow` anyIOException

    context "when operating on layer-3 interfaces" $ do
        it "gets interface ipv4 addresses" . runRTNL . withTestInterface $ do
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [testAddress4]

        it "gets interface ipv6 addresses" . runRTNL . withTestInterface $ do
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [testAddress6]

        it "gets interface indices" . runRTNL . withTestInterface $ do
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [(testAddress4, testIfIndex)]

        it "gets interface prefixes" . runRTNL . withTestInterface $ do
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [(testAddress4, testIfPrefix4)]

        it "gets interface scopes" . runRTNL . withTestInterface $ do
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [(testAddress4, IfUniverse)]

        it "gets interface labels" . runRTNL . withTestInterface $ do
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [(testAddress4, (\(LinkName s) -> IfLabel s) testLink)]

        it "gets interface precedence" . runRTNL . withTestInterface $ do
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [(testAddress4, Primary)]

        it "gets interface duplicate address detection" . runRTNL . withTestInterface $ do
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [(testAddress6, DadEnabled)]

        it "gets interface duplicate address detection flags" . runRTNL . withTestInterface $ do
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [(testAddress6,
                DuplicateAddressDetectionFlags
                    { dadOptimistic = False
                    , dadTentative  = False
                    , dadFailed     = False
                    })]

        it "gets interface MIP6 homing" . runRTNL . withTestInterface $ do
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [(testAddress6, Home)]

        it "gets interface preferences" . runRTNL . withTestInterface $ do
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [(testAddress6, Prefered)]

        it "gets interface permanence" . runRTNL . withTestInterface $ do
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [(testAddress6, Permanent)]

        it "gets interface prefix-route creation" . runRTNL . withTestInterface $ do
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [(testAddress4, PREnabled)]

        it "gets interface multicast autojoin status" . runRTNL . withTestInterface $ do
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [(testAddress4, NoAutoJoin)]

        it "gets interface cache lifetimes" . runRTNL . withTestInterface $ do
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain`
                [(testAddress4, IfLifetime IfForever IfForever)]

testCreate :: Spec
testCreate = do
    context "when operating on layer-2 links" $ do
        it "creates named bridge links" . runRTNL $ do
            create (Bridge, testLink)
            links <- dump AnyLink
            destroy testLink
            liftIO $ links `shouldContain` [testLink]

        it "creates named dummy links" . runRTNL $ do
            create (Dummy, testLink)
            links <- dump AnyLink
            destroy testLink
            liftIO $ links `shouldContain` [testLink]

        it "creates named 802.1Q vlans" . runRTNL . withTestLink $ do
            let vlan = (Dot1QVlan testLinkIndex testVlanId, testVlan)
            create vlan
            links <- dump AnyLink
            liftIO $ links `shouldContain` [vlan]

        it "creates named 802.1ad vlans" . runRTNL . withTestLink $ do
            let vlan = (Dot1adVlan testLinkIndex testVlanId, testVlan)
            create vlan
            links <- dump AnyLink
            liftIO $ links `shouldContain` [vlan]

        it "creates unnamed links" . runRTNL $ do
            before <- dump AnyLink
            create Dummy
            after <- dump AnyLink
            let dummy = head $ after \\ before
            destroy dummy
            liftIO $ dummy `shouldSatisfy`
                \(LinkName name) -> "dummy" `S.isPrefixOf` name

        it "creates numbered links" . runRTNL $ do
            ixs <- dump AnyLink :: RTNL [LinkIndex]
            let ix = maximum ixs + 42
            create (Dummy, ix)
            links <- dump AnyLink
            destroy ix
            liftIO $ links `shouldContain` [ix]

        it "creates links with ethernet addresses" . runRTNL $ do
            let mac = LinkEther 0xaa 0xbb 0xcc 0xdd 0xee 0xff
            create ((Dummy, testLink), mac)
            links <- dump AnyLink
            destroy testLink
            liftIO $ links `shouldContain` [mac]

        it "creates links with ethernet addresses" . runRTNL $ do
            let mac = LinkEther          0xaa 0xbb 0xcc 0xdd 0xee 0xff
                brd = LinkBroadcastEther 0xaa 0xbb 0xcc 0xff 0xff 0xff
            create (((Dummy, testLink), mac), brd)
            links <- dump AnyLink
            destroy testLink
            liftIO $ links `shouldContain` [(mac,brd)]

    context "when operating on layer-3 interfaces" $ do
        it "creates ipv4 addresses" . runRTNL . withTestLink $ do
            let prefix    = 24
                index     = IfIndex $ fromIntegral testLinkIndex
                interface = IfInetAddress testAddress4 prefix index
            create interface
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [testAddress4]

        it "creates ipv6 addresses" . runRTNL . withTestLink $ do
            let prefix    = 64
                index     = IfIndex $ fromIntegral testLinkIndex
                interface = IfInet6Address testAddress6 prefix index
            create interface
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [testAddress6]

        it "creates ipv6 addresses with DAD disabled" . runRTNL . withTestLink $ do
            let prefix    = 64
                index     = IfIndex $ fromIntegral testLinkIndex
                interface = IfInet6Address testAddress6 prefix index
            create (interface, DadDisabled)
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [(testAddress6, DadDisabled)]

        it "creates ipv4 addresses with scopes" . runRTNL . withTestLink $ do
            let prefix    = 24
                index     = IfIndex $ fromIntegral testLinkIndex
                interface = IfInetAddress testAddress4 prefix index
            create (interface, IfHost)
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [(testAddress4, IfHost)]

        it "creates ipv4 addresses with labels" . runRTNL . withTestLink $ do
            let prefix    = 24
                index     = IfIndex $ fromIntegral testLinkIndex
                interface = IfInetAddress testAddress4 prefix index
                label     = IfLabel $ "test-link:foo"
            create (interface, label)
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [(testAddress4, label)]

        it "creates ipv4 addresses without prefix routes" . runRTNL . withTestLink $ do
            let prefix    = 24
                index     = IfIndex $ fromIntegral testLinkIndex
                interface = IfInetAddress testAddress4 prefix index
            create (interface, PRDisabled)
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [(testAddress4, PRDisabled)]

        it "creates ipv6 addresses without prefix routes" . runRTNL . withTestLink $ do
            let prefix    = 64
                index     = IfIndex $ fromIntegral testLinkIndex
                interface = IfInet6Address testAddress6 prefix index
            create (interface, PRDisabled)
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [(testAddress6, PRDisabled)]

        it "creates ipv4 addresses with lifetimes" . runRTNL . withTestLink $ do
            let prefix    = 24
                index     = IfIndex $ fromIntegral testLinkIndex
                interface = IfInetAddress testAddress4 prefix index
                lifetime  = IfLifetime
                    { ifPrefered = IfSeconds 300
                    , ifValid    = IfSeconds 300
                    }
            create (interface, lifetime)
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [testAddress4]

        it "creates ipv6 addresses with lifetimes" . runRTNL . withTestLink $ do
            let prefix    = 64
                index     = IfIndex $ fromIntegral testLinkIndex
                interface = IfInet6Address testAddress6 prefix index
                lifetime  = IfLifetime
                    { ifPrefered = IfSeconds 300
                    , ifValid    = IfSeconds 300
                    }
            create (interface, lifetime)
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldContain` [testAddress6]

        context "when given a bad interface index" $ do
            it "throws an exception" $ do
                ixs <- runRTNL $ dump AnyLink
                let LinkIndex n = maximum ixs + 1
                    badIx       = IfIndex n
                    prefix      = 24
                    interface   = IfInetAddress testAddress4 prefix badIx
                runRTNL (create interface) `shouldThrow` anyIOException

        context "when given a silly prefix" $ do
            it "throws an exception" $ do
                let index     = IfIndex $ fromIntegral testLinkIndex
                    badPrefix = 42
                    interface = IfInetAddress testAddress4 badPrefix index
                runRTNL (withTestLink $ create interface)
                    `shouldThrow` anyIOException

testChange :: Spec
testChange = do
    context "when operating on layer-2 links" $ do
        it "brings links up" . runRTNL . withTestLink $ do
            change testLink Up
            dump' testLink `rtnlShouldReturn` Up

        it "brings links down" . runRTNL . withTestLink $ do
            change testLink Up
            change testLink Down
            dump' testLink `rtnlShouldReturn` Down

        it "makes links promiscuous" . runRTNL . withTestLink $ do
            change testLink Promiscuous
            dump' testLink `rtnlShouldReturn` Promiscuous

        it "makes links chaste" . runRTNL . withTestLink $ do
            change testLink Promiscuous
            change testLink Chaste
            dump' testLink `rtnlShouldReturn` Chaste

        it "turns off arp on links" . runRTNL . withTestLink $ do
            change testLink NoArp
            dump' testLink `rtnlShouldReturn` NoArp

        it "turns on arp on links" . runRTNL . withTestLink $ do
            change testLink NoArp
            change testLink Arp
            dump' testLink `rtnlShouldReturn` Arp

        it "turns off debug on links" . runRTNL . withTestLink $ do
            change testLink Debug
            dump' testLink `rtnlShouldReturn` Debug

        it "turns on arp on links" . runRTNL . withTestLink $ do
            change testLink Debug
            change testLink NoDebug
            dump' testLink `rtnlShouldReturn` NoDebug

        it "changes link MTUs" . runRTNL . withTestLink $ do
            let weirdMTU = LinkMTU 9999
            change testLink weirdMTU
            dump' testLink `rtnlShouldReturn` weirdMTU

        it "changes link ethernet addresses" . runRTNL . withTestLink $ do
            let weirdEther = LinkEther 0xaa 0xbb 0xcc 0xdd 0xee 0xff
            change testLink weirdEther
            dump' testLink `rtnlShouldReturn` weirdEther

        context "when given a non-existent link name" $ do
            it "throws an exception" $ do
                runRTNL (change notALink Up) `shouldThrow` anyIOException

testDestroy :: Spec
testDestroy = do
    context "when operating on layer-2 links" $ do
        it "destroys links by name" . runRTNL $ do
            create (Dummy, testLink)
            destroy testLink
            links <- dump AnyLink
            liftIO $ links `shouldSatisfy` not . elem testLink

        it "destroys links by index" . runRTNL $ do
            create (Dummy, testLinkIndex)
            destroy testLinkIndex
            links <- dump AnyLink
            liftIO $ links `shouldSatisfy` not . elem testLink

        context "when given a non-existent link name" $ do
            it "throws an exception" $ do
                runRTNL (destroy notALink) `shouldThrow` anyIOException

    context "when operating on layer-3 interfaces" $ do
        it "destroys ipv4 addresses" . runRTNL . withTestLink_ $ \(LinkIndex n) -> do
            let index     = IfIndex n
                interface = testIfInetAddress index
            create interface
            destroy interface
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldSatisfy` not . elem testAddress4

        it "destroys ipv6 addresses" . runRTNL . withTestLink_ $ \(LinkIndex n) -> do
            let index     = IfIndex n
                interface = testIfInet6Address index
            create interface
            destroy interface
            addresses <- dump AnyInterface
            liftIO $ addresses `shouldSatisfy` not . elem testAddress6
