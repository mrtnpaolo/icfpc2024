{-# OPTIONS_GHC -Wno-unused-matches #-}
module Main (main) where

import Language.Haskell.TH

import ICFP

import E qualified as E

main =
 do e <- runQ x
    putStrLn "=== hs ===\n"
    print (ppr e)
    putStrLn "\n=== GHC AST ===\n"
    print e
    let a = exp2ast e
    putStrLn "\n=== ICFP.AST ===\n"
    print a
    putStrLn "\n=== ICFP Language ===\n"
    putStrLn (serialize a)

x :: Q Exp

-- x = [| (\v3 -> (\v2 -> v3)) ("Hello" ++ " World") 42 |]

-- (λx.xx)(λx.xx)
-- x = [| (\x -> x x) (\x -> x x) |]

-- factorial (inlined)
--x = [| (\f -> (\x -> f (x x)) (\x -> f (x x))) (\f -> (\n -> if (==) n 0 then 1 else (*) n (f ((-) n 1)))) 10 |]

-- λf.(λx.f(xx))(λx.f(xx))
y = [| (\f -> (\x -> f (x x)) (\x -> f (x x))) |] :: Q Exp

-- factorial
-- f = [| \f -> \n -> if (E.==) n 0 then 1 else (E.*) n (f ((E.-) n 1)) |] :: Q Exp
-- x = [| $E.y $f 10 |]

x = [| $E.e |]

exp2ast :: Exp -> Expr Val
exp2ast = go
  where
   go (ConE (show -> "E.True" )) = T
   go (ConE (show -> "E.False")) = F
   go (LitE (IntegerL n)) = I n
   go (LitE (StringL xs)) = Str xs

   go (AppE (VarE (show -> "GHC.Num.negate")) e) = Neg  (go e)
   go (AppE (VarE (show -> "E.not"         )) e) = Not  (go e)
   go (AppE (VarE (show -> "E.stoi"        )) e) = StoI (go e)
   go (AppE (VarE (show -> "E.itos"        )) e) = ItoS (go e)

   -- turn infix full-applications into prefix applications
   go (InfixE (Just l) f@(VarE fname) (Just r)) = go (AppE (AppE f l) r)

   go (AppE (AppE (VarE fname) e1) e2) =
    case show fname of
      "E.+"    -> Add     (go e1) (go e2)
      "E.-"    -> Sub     (go e1) (go e2)
      "E.*"    -> Mul     (go e1) (go e2)
      "E./"    -> Div     (go e1) (go e2)
      "E.%"    -> Mod     (go e1) (go e2)
      "E.<"    -> Lt      (go e1) (go e2)
      "E.>"    -> Gt      (go e1) (go e2)
      "E.=="   -> Equ     (go e1) (go e2)
      "E.||"   -> Or      (go e1) (go e2)
      "E.&&"   -> And     (go e1) (go e2)
      "E.++"   -> Sconcat (go e1) (go e2)
      "E.take" -> TakeN   (go e1) (go e2)
      "E.drop" -> DropN   (go e1) (go e2)
      xs       -> error (unwords ["unsupported function",show xs])

   go (AppE e1 e2) = App (go e1) (go e2)

   go (CondE e1 e2 e3) = If (go e1) (go e2) (go e3)

   -- TODO: figure out if v_n names are actually unique

   go (LamE [VarP name] body) = Lam (v_n name) (go body)

   go (VarE name) | _:'_':_ <- show name = Var (v_n name)

   go x = error (unwords ["unimplemented",show x])

   v_n :: Name -> Int -- blah_54 -> 54
   v_n = read @Int . reverse . takeWhile ('_'/=) . reverse . show
