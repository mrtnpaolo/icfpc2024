{-# OPTIONS_GHC -Wno-unused-matches #-}
module Main (main) where

import Data.List

import Language.Haskell.TH
--import Language.Haskell.TH.Syntax

import ICFP

main =
 do e <- runQ x
    print (ppr e)
    print e
    let a = exp2ast e
    print a
    putStrLn (serialize a)

x :: Q Exp
x = [| (\v3 -> (\v2 -> v3)) ("Hello" ++ " World") 42 |]

exp2ast :: Exp -> Expr Val
exp2ast = go
  where
   go (AppE e1 e2) = App (go e1) (go e2)

   go (LamE [VarP name] body) = Lam (v_n name) (go body)

   go (VarE name) = Var (v_n name)

   go (InfixE (Just e1) (VarE name) (Just e2))
     | "GHC.Base.++" <- show name = Sconcat (go e1) (go e2)

   go (LitE (StringL xs)) = Str xs

   go (LitE (IntegerL n)) = I (fromIntegral n)

   go x = error (unwords ["unimplemented",show x])

   v_n :: Name -> Int -- blah_54 -> 54
   v_n = read @Int . reverse . takeWhile ('_'/=) . reverse . show
