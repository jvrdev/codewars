module Codewars.Kata.Dec2Fact where

import Data.Char (intToDigit, digitToInt, toUpper, chr, ord, isLetter, isHexDigit)

toDigit :: Integral a => a -> Char
toDigit n
  | n < 16 = toUpper . intToDigit . fromIntegral $ n
  | otherwise = chr $ (ord 'A') - 10 + (fromIntegral n)

fromDigit :: Integral a => Char -> a
fromDigit c
  | isHexDigit c = fromIntegral $ digitToInt c
  | isLetter c = fromIntegral $ ord (toUpper c) - (ord 'A') + 10

dec2FactString :: Integer -> String
dec2FactString = reverse . go 1
  where
    go _ 0 = []
    go i m = (toDigit $ m `rem` i) : (go (i + 1) (m `div` i))

factString2Dec :: String -> Integer
factString2Dec =
  sum . zipWith (*) facs . reverse . map fromDigit
  where
    facs = scanl (*) 1 [1..]
