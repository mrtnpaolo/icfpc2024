module ICFP.Eval (eval) where

import ICFP.Base94
import ICFP.AST

eval :: Expr Val -> Val

eval T = VB True
eval F = VB False

eval (I n) = VI n

eval (Str xs) = VS xs

eval (Neg e) = VI $ negate (valI (eval e))
eval (Not e) = VB $ not (valB (eval e))

eval (StoI e) = VI $ readBase94 $ valS (eval e)
eval (ItoS e) = VS $ decode94 $ showBase94 $ valI (eval e)

eval (Add e1 e2) = VI $ (+)   (valI (eval e1)) (valI (eval e2))
eval (Sub e1 e2) = VI $ (-)   (valI (eval e1)) (valI (eval e2))
eval (Mul e1 e2) = VI $ (*)   (valI (eval e1)) (valI (eval e2))
eval (Div e1 e2) = VI $ (div) (valI (eval e1)) (valI (eval e2))
eval (Mod e1 e2) = VI $ (mod) (valI (eval e1)) (valI (eval e2))

eval (Lt e1 e2) = VB $ (<) (valI (eval e1)) (valI (eval e2))
eval (Gt e1 e2) = VB $ (>) (valI (eval e1)) (valI (eval e2))
eval (Equ e1 e2)
  | VB a <- v1, VB b <- v2 = VB $ (==) a b
  | VI a <- v1, VI b <- v2 = VB $ (==) a b
  | VS a <- v1, VS b <- v2 = VB $ (==) a b
  where
    v1 = eval e1; v2 = eval e2

eval (Or  e1 e2) = VB $ (||) (valB (eval e1)) (valB (eval e2))
eval (And e1 e2) = VB $ (&&) (valB (eval e1)) (valB (eval e2))

eval (Sconcat e1 e2) = VS $ (++) (valS (eval e1)) (valS (eval e2))

eval (TakeN e1 e2) = VS $ take (valI (eval e1)) (valS (eval e2))
eval (DropN e1 e2) = VS $ drop (valI (eval e1)) (valS (eval e2))

-- TODO: Apply
-- TODO: If
-- TODO: Lam
-- TODO: Var

eval _ = error "unimplemented"
