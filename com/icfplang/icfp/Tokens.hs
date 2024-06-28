module ICFP.Tokens where

import ICFP.Base94

data Token

  -- data
  = TT
  | TF
  | TI Int
  | TStr String

  -- unary ops
  | TNeg | TNot | TStoI | TItoS

  -- binary ops
  | TAdd | TSub | TMul | TDiv | TMod | TLt | TGt | TEqu | TOr | TAnd | TSconcat | TTakeN | TDropN

  -- lambdas
  | TApply | TIf | TLam Int | TVar Int

  deriving (Show)

-- tokenizer

tokenize = map token . words

token "T" = TT

token "F" = TF

token ('I':xs) = TI (readBase94 xs)

token ('S':xs) = TStr (decode94 xs)

token ("U-") = TNeg
token ("U!") = TNot
token ("U#") = TStoI
token ("U$") = TItoS
token ('U':x) = error (unwords ["unrecognized unary op",x])

token ("B+") = TAdd
token ("B-") = TSub
token ("B*") = TMul
token ("B/") = TDiv
token ("B%") = TMod
token ("B<") = TLt
token ("B>") = TGt
token ("B=") = TEqu
token ("B|") = TOr
token ("B&") = TAnd
token ("B.") = TSconcat
token ("BT") = TTakeN
token ("BD") = TDropN
token ("B$") = TApply
token ('B':x) = error (unwords ["unrecotnized binary op",x])

token ("?") = TIf
token ('?':x) = error (unwords ["spurious chars in if",x])

token ('L':xs) = TLam n
  where
    TI n = token ('I':xs)

token ('v':xs) = TVar n
  where
    TI n = token ('I':xs)

token x = error (unwords ["unrecognized token",x])
