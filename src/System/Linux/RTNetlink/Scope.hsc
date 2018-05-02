{-|
Module      : System.Linux.RTNetlink.Scope
Description : Address or route scope
Copyright   : (c) Formaltech Inc. 2017
License     : BSD3
Maintainer  : protob3n@gmail.com
Stability   : experimental
Portability : Linux
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module System.Linux.RTNetlink.Scope (
    Scope(..)
  , RtScopeEnum(..)
  , fromScope
  , toScope
  ) where

import Foreign.C.Types (CInt)

#include <linux/rtnetlink.h>

-- | Adress and route scope
newtype RtScopeEnum = RtScopeEnum { fromRtScopeEnum :: CInt }
 deriving (Eq, Show, Num)
#{enum RtScopeEnum, RtScopeEnum, RT_SCOPE_UNIVERSE, RT_SCOPE_SITE, RT_SCOPE_LINK, RT_SCOPE_HOST, RT_SCOPE_NOWHERE}

data Scope =
    ScopeUniverse
  | ScopeSite
  | ScopeLink
  | ScopeHost
  | ScopeNowhere
 deriving (Eq, Ord)

instance Show Scope where
  show ScopeUniverse = "global"
  show ScopeSite = "site"
  show ScopeLink = "link"
  show ScopeHost = "host"
  show ScopeNowhere = "nowhere"

fromScope :: Scope -> RtScopeEnum
fromScope ScopeUniverse = rtScopeUniverse
fromScope ScopeSite = rtScopeSite
fromScope ScopeLink = rtScopeLink
fromScope ScopeHost = rtScopeHost
fromScope ScopeNowhere = rtScopeNowhere

toScope :: RtScopeEnum -> Scope
toScope x | x == rtScopeUniverse = ScopeUniverse
toScope x | x == rtScopeSite = ScopeSite
toScope x | x == rtScopeLink = ScopeLink
toScope x | x == rtScopeHost = ScopeHost
toScope x | x == rtScopeNowhere = ScopeNowhere
toScope _ = error "No such route scope"
