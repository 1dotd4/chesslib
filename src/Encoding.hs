{-# LANGUAGE OverloadedStrings, DeriveGeneric, BangPatterns #-}
module Encoding where

import GHC.Generics
-- import Data.Aeson as JSON
import qualified Data.Maybe as DM
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.List.Extra as E

convertToAscii :: Int -> Char
convertToAscii = toEnum

convertArrayToAscii :: [Int] -> String
convertArrayToAscii = map convertToAscii

onlyPrintable :: BS.ByteString -> Either BS.ByteString BS.ByteString
onlyPrintable = f . BS.unpack
  where
    f s = r $ foldl g (Right []) s
    r (Left x)  = Left  $ BS.pack x
    r (Right x) = Right $ BS.pack x
    g (Left _) _ = Left []
    g (Right a) b
      | p b       = Right (a ++ [b])
      | otherwise = Left []
    p a = (fromIntegral a) > 32 && (fromIntegral a) < 126

hex :: [Char] -> [Char]
hex a = foldl (<>) "" $ map w a
  where w ch = let s = "0123456789ABCDEF"
                   x = fromEnum ch
               in [s !! div x 16,s !! mod x 16]

unhex :: [Char] -> BS.ByteString
unhex a = convert $ foldl (<>) [] $ DM.maybeToList . c <$> a
  where
    convert (x:y:r) = BS.append (BS.pack [(fromIntegral ((x * 16) + y))]) $ convert r
    convert [] = BS.empty
    convert [_] = BS.empty
    -- c :: Char -> Maybe Int
    c '0' = Just 0
    c '1' = Just 1
    c '2' = Just 2
    c '3' = Just 3
    c '4' = Just 4
    c '5' = Just 5
    c '6' = Just 6
    c '7' = Just 7
    c '8' = Just 8
    c '9' = Just 9
    c 'A' = Just 10
    c 'B' = Just 11
    c 'C' = Just 12
    c 'D' = Just 13
    c 'E' = Just 14
    c 'F' = Just 15
    c 'a' = Just 10
    c 'b' = Just 11
    c 'c' = Just 12
    c 'd' = Just 13
    c 'e' = Just 14
    c 'f' = Just 15
    c _   = Nothing

unbase64 = BS64.decodeBase64Lenient

fromHexToInt a = foldl sum 0 a
  where sum a b = 16 * a + (c b)
        c :: Char -> Integer
        c '0' = 0
        c '1' = 1
        c '2' = 2
        c '3' = 3
        c '4' = 4
        c '5' = 5
        c '6' = 6
        c '7' = 7
        c '8' = 8
        c '9' = 9
        c 'A' = 10
        c 'B' = 11
        c 'C' = 12
        c 'D' = 13
        c 'E' = 14
        c 'F' = 15
        c 'a' = 10
        c 'b' = 11
        c 'c' = 12
        c 'd' = 13
        c 'e' = 14
        c 'f' = 15

fromIntToHex :: Integer -> String
fromIntToHex a = d a ""
  where d 0 b = b
        d x y = d (div x 256) (flip (<>) y $ w (fromInteger (mod x 256)))
        w ch = let s = "0123456789ABCDEF"
               in [s !! div ch 16, s !! mod ch 16]

longToBytes = unhex . fromIntToHex
bytesToLong = fromHexToInt . hex
