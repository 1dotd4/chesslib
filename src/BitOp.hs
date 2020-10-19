module BitOp where

import qualified Data.ByteString as BS
import qualified Data.Bits       as B

rot13 x
  | (fromIntegral x) - 97 < 26 && (fromIntegral x) - 97 >= 0 = 97 + rem (x - 84) 26
  | (fromIntegral x) - 65 < 26 && (fromIntegral x) - 65 >= 0 = 65 + rem (x - 52) 26
  | otherwise = x

xorWithInt s i = BS.pack  $ (B.xor (fromIntegral i)) 
                         <$> BS.unpack s

xorStrings a b = BS.pack $ xorArrays (BS.unpack a) (BS.unpack b)
  where
    xorArrays (a:x) (b:y) = ( B.xor a b : xorArrays x y )
    xorArrays [] _ = []
    xorArrays _ [] = []

xorRepeatingKey a k = BS.pack $ xorKey (BS.unpack a) (BS.unpack k)
  where
    xorKey (a:b) (x:y) = (B.xor a x : xorKey b (y ++ [x]))
    xorKey [] _ = []
