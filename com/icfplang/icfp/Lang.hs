module ICFP.Lang where

import Data.Char (ord,chr)
import Numeric (readInt)

data L
  = T
  | F
  | I Int
  | Str String
  -- unary ops
  | Neg | Not | StoI | ItoS
  -- binary ops
  | Add | Sub | Mul | Div | Mod | Lt | Gt | Equ | Bor | Band | Sconcat | TakeN | DropN | Apply
  | If
  | Lam Int
  | Var Int
  deriving (Show)

dec = unwords . map show . map token . words

-- indicators: TFISUB?Lv
--     parsed: #########
--  evaluated: .........

token "T" = T
token "F" = F

token ('I':xs) = I n
  where
    [(n,"")] = readInt 94 valid fromChar xs
    valid (ord -> n) = 33 <= n && n <= 126
    fromChar (ord -> n) = n - 33

token ('S':xs) = Str (map (f . ord) xs)
  where
    f n | n < 33 || n > 126 = chr n
        | otherwise = alphabet !! (n - 33)
    alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

token ("U-") = Neg
token ("U!") = Not
token ("U#") = StoI
token ("U$") = ItoS
token ('U':x) = error (unwords ["unrecognized unary op",x])

token ("B+") = Add
token ("B-") = Sub
token ("B*") = Mul
token ("B/") = Div
token ("B%") = Mod
token ("B<") = Lt
token ("B>") = Gt
token ("B=") = Equ
token ("B|") = Bor
token ("B&") = Band
token ("B.") = Sconcat
token ("BT") = TakeN
token ("BD") = DropN
token ("B$") = Apply
token ('B':x) = error (unwords ["unrecotnized binary op",x])

token ("?") = If
token ('?':x) = error (unwords ["spurious chars in if",x])

token ('L':xs) = Lam n
  where
    I n = token ('I':xs)

token ('v':xs) = Var n
  where
    I n = token ('I':xs)

token x = error (unwords ["unrecognized token",x])
