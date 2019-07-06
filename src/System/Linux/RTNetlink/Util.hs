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

import Data.Bits (Bits(complement, zeroBits, (.&.), (.|.), shiftL, shiftR))
import Data.Bits (FiniteBits(finiteBitSize))
import Data.Serialize (Serialize, Get, runGet, decode)
import Data.Word (Word8, Word16)
import Numeric (showHex)
import qualified Data.ByteString as S

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

oneBits :: Bits a => a
oneBits = complement zeroBits

decodeMaybe :: Serialize a => S.ByteString -> Maybe a
decodeMaybe = either (const Nothing) Just . decode

runGetMaybe :: Get a -> S.ByteString -> Maybe a
runGetMaybe g = either (const Nothing) Just . runGet g

byteSwap16 :: Word16 -> Word16
byteSwap16 b = ((b .&. 0x00ff) `shiftL` 8) .|. ((b .&. 0xff00) `shiftR` 8)
