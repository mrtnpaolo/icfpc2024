module ICFP.Base94 where

import Numeric (readInt,showIntAtBase)
import Data.Char (ord,chr)

readBase94 xs = n
  where
    [(n,"")] = readInt 94 valid fromChar xs
    valid (ord -> n) = 33 <= n && n <= 126
    fromChar (ord -> n) = n - 33

showBase94 n = showIntAtBase 94 (chr . (33+)) n ""

decode94 = map (f . ord)
  where
    f n | n < 33 || n > 126 = chr n
        | otherwise = alphabet !! (n - 33)
    alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"
