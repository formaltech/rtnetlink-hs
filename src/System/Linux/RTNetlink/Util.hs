{-|
Module      : System.Linux.RTNetlink.Util
Description : Internal utility functions.
Copyright   : (c) Formaltech Inc. 2017
License     : BSD3
Maintainer  : protob3n@gmail.com
Stability   : experimental
Portability : Linux
-}
module System.Linux.RTNetlink.Util where

import Data.Word (Word8)
import Numeric (showHex)

left :: (a -> b) -> Either a c -> Either b c
left f = either (Left . f) Right

unfoldM :: Monad m => m (Maybe a) -> m [a]
unfoldM m = do
    mb <- m
    case mb of
        Nothing -> return []
        Just a  -> (a:) <$> unfoldM m

showMac :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> String
showMac a b c d e f = hex a <:> hex b <:> hex c <:> hex d <:> hex e <:> hex f
    where
    hex w   = showHex w ""
    s <:> t = s ++ ":" ++ t :: String
