{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (bracket)
import Data.Functor ((<$>))
import System.Posix (getEffectiveUserID)
import System.Socket.Family.Inet (inetAddressFromTuple)
import System.Socket.Family.Inet6 (inet6AddressFromTuple)

import Test.Hspec

import System.Linux.RTNetlink
import System.Linux.RTNetlink.Link
import System.Linux.RTNetlink.Address

loopback :: LinkName
loopback = "lo"

testLink :: LinkName
testLink = "foobazblargle"

notALink :: LinkName
notALink = "notalink"

localhost4 :: InetAddress
localhost4 = inetAddressFromTuple (127,0,0,1)

localhost6 :: Inet6Address
localhost6 = inet6AddressFromTuple (0,0,0,0,0,0,0,1)

testAddress4 :: InetAddress
testAddress4 = inetAddressFromTuple (169,254,42,42)

testAddress6 :: Inet6Address
testAddress6 = inet6AddressFromTuple (0xfe80,42,42,42,42,42,42,42)

createTestInterface :: IO ()
createTestInterface = runRTNL $ do
    create $ Dummy testLink
    [LinkIndex n] <- dump testLink
    let prefix4 = 24
        prefix6 = 64
        index   = IfIndex n
    create $ IfInetAddress testAddress4 prefix4 index
    create $ IfInet6Address testAddress6 prefix6 index

withTestInterface :: IO a -> IO a
withTestInterface = bracket createTestInterface (const destroyTestLink) . const

createTestLink :: IO ()
createTestLink = runRTNL (create $ Dummy testLink)

destroyTestLink :: IO ()
destroyTestLink = runRTNL $ destroy testLink

withTestLink :: IO a -> IO a
withTestLink = bracket createTestLink (const destroyTestLink) . const

main :: IO ()
main = do
    haveRoot <- (0 ==) <$> getEffectiveUserID
    hspec $ do
        describe "dump" testDump
        describe "create" $
            if haveRoot
                then testCreate
                else it "should create things" $ pendingWith "requires root"
        describe "change" $
            if haveRoot
                then testChange
                else it "should change things" $ pendingWith "requires root"
        describe "destroy" $
            if haveRoot
                then testDestroy
                else it "should destroy things" $ pendingWith "requires root"

testDump :: Spec
testDump = do
    context "when operating on layer-2 links" $ do
        it "gets link names" $ do
            links <- runRTNL $ dump AnyLink
            links `shouldSatisfy` elem loopback

        it "gets link indices" $ do
            runRTNL (dump loopback) `shouldReturn` [LinkIndex 1]

        it "gets link ethernet addresses" $ do
            runRTNL (dump loopback) `shouldReturn` [LinkEther 0 0 0 0 0 0]

        it "gets link broadcast ethernet addresses" $ do
            runRTNL (dump loopback) `shouldReturn` [LinkBroadcastEther 0 0 0 0 0 0]

        it "gets link states" $ do
            runRTNL (dump loopback) `shouldReturn` [Up]

        it "gets link promiscuity" $ do
            runRTNL (dump loopback) `shouldReturn` [Chaste]

        it "gets link arp state" $ do
            runRTNL (dump loopback) `shouldReturn` [Arp]

        it "gets link debug state" $ do
            runRTNL (dump loopback) `shouldReturn` [NoDebug]

        it "gets link MTUs" $ do
            runRTNL (dump loopback) `shouldReturn` [LinkMTU 0x10000]

        context "when given a non-existent link name" $ do
            it "throws an exception" $ do
                runRTNL (dump notALink :: RTNL [()]) `shouldThrow` anyIOException

    context "when operating on layer-3 interfaces" $ do
        it "gets interface ipv4 addresses" $ do
            addresses <- runRTNL $ dump AnyInterface
            addresses `shouldSatisfy` elem localhost4

        it "gets interface ipv6 addresses" $ do
            addresses <- runRTNL $ dump AnyInterface
            addresses `shouldSatisfy` elem localhost6

testCreate :: Spec
testCreate = do
    context "when operating on layer-2 links" $ after_ destroyTestLink $ do
        it "creates bridge links" $ do
            links <- runRTNL $ do
                create $ Bridge testLink
                dump AnyLink
            links `shouldSatisfy` elem testLink

        it "creates dummy links" $ do
            links <- runRTNL $ do
                create $ Dummy testLink
                dump AnyLink
            links `shouldSatisfy` elem testLink

        it "creates vlan links" $ do
            let testVlan = LinkName "zipzapvlan"
            links <- runRTNL $ do
                create $ Dummy testLink
                ix:_ <- dump testLink
                create $ Dot1QVlan ix 101 testVlan
                dump AnyLink
            links `shouldSatisfy` elem testVlan

    context "when operating on layer-3 interfaces" $ around_ withTestLink $ do
        it "creates ipv4 addresses" $ do
            addresses <- runRTNL $ do
                [LinkIndex n] <- dump testLink
                let prefix    = 24
                    index     = IfIndex n
                    interface = IfInetAddress testAddress4 prefix index
                create interface
                dump AnyInterface
            addresses `shouldSatisfy` elem testAddress4

        it "creates ipv6 addresses" $ do
            addresses <- runRTNL $ do
                [LinkIndex n] <- dump testLink
                let prefix    = 64
                    index     = IfIndex n
                    interface = IfInet6Address testAddress6 prefix index
                create interface
                dump AnyInterface
            addresses `shouldSatisfy` elem testAddress6

        context "when given a bad interface index" $ do
            it "throws an exception" $ do
                indices <- runRTNL $ dump AnyLink
                let LinkIndex n = maximum indices + 1
                    badIx       = IfIndex n
                    prefix      = 24
                    interface   = IfInetAddress testAddress4 prefix badIx
                runRTNL (create interface) `shouldThrow` anyIOException

        context "when given a silly prefix" $ do
            it "throws an exception" $ do
                [LinkIndex n] <- runRTNL $ dump testLink
                let index     = IfIndex n
                    badPrefix = 42
                    interface = IfInetAddress testAddress4 badPrefix index
                runRTNL (create interface) `shouldThrow` anyIOException

testChange :: Spec
testChange = do
    context "when operating on layer-2 links" $ around_ withTestLink $ do
        it "brings links up" $ do
            [state] <- runRTNL $ do
                change testLink Up
                dump testLink
            state `shouldBe` Up

        it "brings links down" $ do
            [state] <- runRTNL $ do
                change testLink Up
                change testLink Down
                dump testLink
            state `shouldBe` Down

        it "makes links promiscuous" $ do
            [state] <- runRTNL $ do
                change testLink Promiscuous
                dump testLink
            state `shouldBe` Promiscuous

        it "makes links chaste" $ do
            [state] <- runRTNL $ do
                change testLink Promiscuous
                change testLink Chaste
                dump testLink
            state `shouldBe` Chaste

        it "turns off arp on links" $ do
            [state] <- runRTNL $ do
                change testLink NoArp
                dump testLink
            state `shouldBe` NoArp

        it "turns on arp on links" $ do
            [state] <- runRTNL $ do
                change testLink NoArp
                change testLink Arp
                dump testLink
            state `shouldBe` Arp

        it "turns off debug on links" $ do
            [state] <- runRTNL $ do
                change testLink Debug
                dump testLink
            state `shouldBe` Debug

        it "turns on arp on links" $ do
            [state] <- runRTNL $ do
                change testLink Debug
                change testLink NoDebug
                dump testLink
            state `shouldBe` NoDebug

        it "changes link MTUs" $ do
            let weirdMTU = LinkMTU 9999
            [mtu] <- runRTNL $ do
                change testLink weirdMTU
                dump testLink
            mtu `shouldBe` weirdMTU

        it "changes link ethernet addresses" $ do
            let weirdEther = LinkEther 0xaa 0xbb 0xcc 0xdd 0xee 0xff
            [eth] <- runRTNL $ do
                change testLink weirdEther
                dump testLink
            eth `shouldBe` weirdEther

        context "when given a non-existent link name" $ do
            it "throws an exception" $ do
                runRTNL (change notALink Up) `shouldThrow` anyIOException

testDestroy :: Spec
testDestroy = do
    context "when operating on layer-2 links" $ before_ createTestLink $ do
        it "destroys links by name" $ do
            links <- runRTNL $ do
                destroy testLink
                dump AnyLink
            links `shouldSatisfy` not . elem testLink

        it "destroys links by index" $ do
            links <- runRTNL $ do
                [LinkIndex n] <- dump testLink
                destroy $ LinkIndex n
                dump AnyLink
            links `shouldSatisfy` not . elem testLink

        context "when given a non-existent link name" $ after_ destroyTestLink $ do
            it "throws an exception" $ do
                runRTNL (destroy notALink) `shouldThrow` anyIOException

    context "when operating on layer-3 interfaces" $ around_ withTestInterface $ do
        it "destroys ipv4 addresses" $ do
            addresses <- runRTNL $ do
                [LinkIndex n] <- dump testLink
                let prefix    = 24
                    index     = IfIndex n
                    interface = IfInetAddress testAddress4 prefix index
                destroy interface
                dump AnyInterface
            addresses `shouldSatisfy` not . elem testAddress4

        it "destroys ipv6 addresses" $ do
            addresses <- runRTNL $ do
                [LinkIndex n] <- dump testLink
                let prefix    = 64
                    index     = IfIndex n
                    interface = IfInet6Address testAddress6 prefix index
                destroy interface
                dump AnyInterface
            addresses `shouldSatisfy` not . elem testAddress6
