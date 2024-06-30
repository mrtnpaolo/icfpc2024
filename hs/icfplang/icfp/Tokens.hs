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
  | TApp | TIf | TLam Int | TVar Int

  deriving (Show)

-- tokenizer

tokenize = map token . words

token "T" = TT

token "F" = TF

token ('I':xs) = TI (readBase94 xs)

token ('S':xs) = TStr xs

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
token ("B$") = TApp
token ('B':x) = error (unwords ["unrecognized binary op",x])

token ("?") = TIf
token ('?':x) = error (unwords ["spurious chars in if",x])

token ('L':xs) = TLam (readBase94 xs)

token ('v':xs) = TVar (readBase94 xs)

token x = error (unwords ["unrecognized token",x])
