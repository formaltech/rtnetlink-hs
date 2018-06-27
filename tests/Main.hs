{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import System.Linux.RTNetlink.Scope

import System.Linux.Namespaces

loopback :: LinkName
loopback = LinkName "lo"

testLink :: LinkName
testLink = LinkName "foobazblargle"

notALink :: LinkName
notALink = LinkName "notalink"

testAddress4 :: InetAddress
testAddress4 = inetAddressFromTuple (169,254,42,42)

testAddress6 :: Inet6Address
testAddress6 = inet6AddressFromTuple (0xfe80,42,42,42,42,42,42,42)

createTestInterface :: IO ()
createTestInterface = runRTNL $ do
    create $ Bridge testLink
    [LinkIndex n] <- dump testLink
    let prefix4 = IfPrefix 24
        prefix6 = IfPrefix 64
        index   = IfIndex n
        scope   = IfScope ScopeLink
    create $ IfInetAddress testAddress4 prefix4 index scope
    create $ IfInet6Address testAddress6 prefix6 index scope

withTestInterface :: IO a -> IO a
withTestInterface = bracket createTestInterface (const destroyTestLink) . const

createTestLink :: IO ()
createTestLink = runRTNL (create $ Bridge testLink)

destroyTestLink :: IO ()
destroyTestLink = runRTNL $ destroy testLink

withTestLink :: IO a -> IO a
withTestLink = bracket createTestLink (const destroyTestLink) . const

main :: IO ()
main = do
    uid <- getEffectiveUserID
    unshare [User, Network]
    writeUserMappings Nothing [UserMapping 0 uid 1]
    hspec $ do
        describe "dump"    testDump
        describe "create"  testCreate
        describe "change"  testChange
        describe "destroy" testDestroy

testDump :: Spec
testDump = do
    context "when operating on layer-2 links" $ do
        it "gets link names" $ do
            links <- runRTNL $ dump AnyLink
            links `shouldSatisfy` elem loopback

        it "gets link indices" $ do
            runRTNL (dump loopback) `shouldReturn` [LinkIndex 1]

        it "gets link states" $ do
            runRTNL (dump loopback) `shouldReturn` [Down]

        context "when given a non-existent link name" $ do
            it "throws an exception" $ do
                runRTNL (dump notALink :: RTNL [()]) `shouldThrow` anyIOException

    context "when operating on layer-3 interfaces" $ do
        it "gets link ethernet addresses" $ do
            runRTNL (dump loopback) `shouldReturn` [LinkEther 0 0 0 0 0 0]

        it "gets interface ipv4 addresses" $ do
            addresses :: [IfInetAddress] <- runRTNL $ dump AnyInterface
            addresses `shouldSatisfy` null

        it "gets interface ipv6 addresses" $ do
            addresses :: [IfInet6Address] <- runRTNL $ dump AnyInterface
            addresses `shouldSatisfy` null

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

    context "when operating on layer-3 interfaces" $ around_ withTestLink $ do
        it "creates ipv4 addresses" $ do
            addresses <- runRTNL $ do
                [LinkIndex n] <- dump testLink
                let prefix    = IfPrefix 24
                    index     = IfIndex n
                    scope     = IfScope ScopeLink
                    interface = IfInetAddress testAddress4 prefix index scope
                create interface
                dump AnyInterface
            addresses `shouldSatisfy` elem testAddress4

        it "creates ipv6 addresses" $ do
            addresses <- runRTNL $ do
                [LinkIndex n] <- dump testLink
                let prefix    = IfPrefix 64
                    index     = IfIndex n
                    scope     = IfScope ScopeLink
                    interface = IfInet6Address testAddress6 prefix index scope
                create interface
                dump AnyInterface
            addresses `shouldSatisfy` elem testAddress6

        context "when given a bad interface index" $ do
            it "throws an exception" $ do
                indices <- runRTNL $ dump AnyLink
                let LinkIndex n = maximum indices + 1
                    badIx       = IfIndex n
                    prefix      = IfPrefix 24
                    scope       = IfScope ScopeLink
                    interface   = IfInetAddress testAddress4 prefix badIx scope
                runRTNL (create interface) `shouldThrow` anyIOException

        context "when given a silly prefix" $ do
            it "throws an exception" $ do
                [LinkIndex n] <- runRTNL $ dump testLink
                let index     = IfIndex n
                    badPrefix = IfPrefix 42
                    scope     = IfScope ScopeLink
                    interface = IfInetAddress testAddress4 badPrefix index scope
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
                let prefix    = IfPrefix 24
                    index     = IfIndex n
                    scope     = IfScope ScopeLink
                    interface = IfInetAddress testAddress4 prefix index scope
                destroy interface
                dump AnyInterface
            addresses `shouldSatisfy` not . elem testAddress4

        it "destroys ipv6 addresses" $ do
            addresses <- runRTNL $ do
                [LinkIndex n] <- dump testLink
                let prefix    = IfPrefix 64
                    index     = IfIndex n
                    scope     = IfScope ScopeLink
                    interface = IfInet6Address testAddress6 prefix index scope
                destroy interface
                dump AnyInterface
            addresses `shouldSatisfy` not . elem testAddress6
