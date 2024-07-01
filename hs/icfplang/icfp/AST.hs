module ICFP.AST where

import ICFP.Base94
import ICFP.Tokens

import Data.Monoid

data Val = VB Bool | VI Integer | VS String | VF (Val -> Val)

instance Show Val where
  show = val

valB (VB x) = x
valI (VI x) = x
valS (VS x) = x
valF (VF f) = f

val (VB x) = show x
val (VI x) = show x
val (VS x) = x
val (VF _) = "<function>"

data Expr a where
  T :: Expr Val
  F :: Expr Val
  I :: Integer -> Expr Val
  Str :: String -> Expr Val

  -- unary ops
  Neg :: Expr Val -> Expr Val
  Not :: Expr Val -> Expr Val

  StoI :: Expr Val -> Expr Val
  ItoS :: Expr Val -> Expr Val

  -- binary ops
  Add :: Expr Val -> Expr Val -> Expr Val
  Sub :: Expr Val -> Expr Val -> Expr Val
  Mul :: Expr Val -> Expr Val -> Expr Val
  Div :: Expr Val -> Expr Val -> Expr Val
  Mod :: Expr Val -> Expr Val -> Expr Val

  Lt  :: Expr Val -> Expr Val -> Expr Val
  Gt  :: Expr Val -> Expr Val -> Expr Val
  Equ :: Expr Val -> Expr Val -> Expr Val

  Or  :: Expr Val -> Expr Val -> Expr Val
  And :: Expr Val -> Expr Val -> Expr Val

  Sconcat :: Expr Val -> Expr Val -> Expr Val

  TakeN :: Expr Val -> Expr Val -> Expr Val
  DropN :: Expr Val -> Expr Val -> Expr Val

  App :: Expr Val -> Expr Val -> Expr Val
  If :: Expr Val -> Expr Val -> Expr Val -> Expr Val
  Lam :: Int -> Expr Val -> Expr Val
  Var :: Int -> Expr Val

deriving instance Show a => Show (Expr a)

parse :: [Token] -> (Expr Val,[Token])

parse (TT : r) = (T,r)
parse (TF : r) = (F,r)

parse (TI n : r) = (I n,r)
parse (TStr xs : r) = (Str xs,r)

parse (TNeg : r) = (Neg e,r1) where (e,r1) = parse r
parse (TNot : r) = (Not e,r1) where (e,r1) = parse r

parse (TStoI : r) = (StoI e,r1) where (e,r1) = parse r
parse (TItoS : r) = (ItoS e,r1) where (e,r1) = parse r

parse (TAdd : r) = bin Add r
parse (TSub : r) = bin Sub r
parse (TMul : r) = bin Mul r
parse (TDiv : r) = bin Div r
parse (TMod : r) = bin Mod r

parse (TLt  : r) = bin Lt  r
parse (TGt  : r) = bin Gt  r
parse (TEqu : r) = bin Equ r

parse (TOr  : r) = bin Or  r
parse (TAnd : r) = bin And r

parse (TSconcat : r) = bin Sconcat r

parse (TTakeN : r) = bin TakeN r
parse (TDropN : r) = bin DropN r

parse (TApp : r) = bin App r

parse (TIf : r) = ter If r

parse (TLam n : r) = (Lam n e,r1) where (e,r1) = parse r

parse (TVar n : r) = (Var n,r)

parse x = error (unwords ["cannot parse",show x])

bin k r = (k e1 e2,r2) where (e1,r1) = parse r; (e2,r2) = parse r1

ter k r = (k e1 e2 e3,r3) where (e1,r1) = parse r; (e2,r2) = parse r1; (e3,r3) = parse r2

-- serialize to text

serialize :: Expr Val -> String
serialize e = unwords (go e `appEndo` [])
  where
    go :: Expr Val -> Endo [String]

    go (T) = Endo ("T":)
    go (F) = Endo ("F":)
    go (I n) = Endo (('I':showBase94 n):)
    go (Str xs) = Endo (('S':encode94 xs):)

    go (Neg e) = Endo ("U-":) <> go e
    go (Not e) = Endo ("U!":) <> go e

    go (StoI e) = Endo ("U#":) <> go e
    go (ItoS e) = Endo ("U$":) <> go e

    go (Add e1 e2) = Endo ("B+":) <> go e1 <> go e2
    go (Sub e1 e2) = Endo ("B-":) <> go e1 <> go e2
    go (Mul e1 e2) = Endo ("B*":) <> go e1 <> go e2
    go (Div e1 e2) = Endo ("B/":) <> go e1 <> go e2
    go (Mod e1 e2) = Endo ("B%":) <> go e1 <> go e2

    go (Lt  e1 e2) = Endo ("B<":) <> go e1 <> go e2
    go (Gt  e1 e2) = Endo ("B>":) <> go e1 <> go e2
    go (Equ e1 e2) = Endo ("B=":) <> go e1 <> go e2

    go (Sconcat e1 e2) = Endo ("B.":) <> go e1 <> go e2

    go (TakeN e1 e2) = Endo ("BT":) <> go e1 <> go e2
    go (DropN e1 e2) = Endo ("BD":) <> go e1 <> go e2

    go (App e1 e2) = Endo ("B$":) <> go e1 <> go e2

    go (If e1 e2 e3) = Endo ("?":) <> go e1 <> go e2 <> go e3

    go (Lam n e) = Endo (('L':encode94 (show n)):) <> go e
    go (Var n) = Endo (('v':encode94 (show n)):)
