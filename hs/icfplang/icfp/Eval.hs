module ICFP.Eval where

import ICFP.Base94
import ICFP.AST

import Data.IntMap.Lazy qualified as IM

type Env = IM.IntMap Val
emptyEnv = IM.empty
bind env n e = IM.insert n e env
resolve = (IM.!)

eval :: Env -> Expr Val -> Val

eval _ T = VB True
eval _ F = VB False

eval _ (I n) = VI n

eval _ (Str xs) = VS xs

eval env (Neg e) = VI $ negate (valI (eval env e))
eval env (Not e) = VB $ not (valB (eval env e))

eval env (StoI e) = VI $ readBase94 $ valS (eval env e)
eval env (ItoS e) = VS $ showBase94 $ valI (eval env e)

eval env (Add e1 e2) = VI $ (+)  (valI (eval env e1)) (valI (eval env e2))
eval env (Sub e1 e2) = VI $ (-)  (valI (eval env e1)) (valI (eval env e2))
eval env (Mul e1 e2) = VI $ (*)  (valI (eval env e1)) (valI (eval env e2))
eval env (Div e1 e2) = VI $ quot (valI (eval env e1)) (valI (eval env e2))
eval env (Mod e1 e2) = VI $ rem  (valI (eval env e1)) (valI (eval env e2))

eval env (Lt e1 e2) = VB $ (<) (valI (eval env e1)) (valI (eval env e2))
eval env (Gt e1 e2) = VB $ (>) (valI (eval env e1)) (valI (eval env e2))
eval env (Equ e1 e2)
  | VB a <- v1, VB b <- v2 = VB $ (==) a b
  | VI a <- v1, VI b <- v2 = VB $ (==) a b
  | VS a <- v1, VS b <- v2 = VB $ (==) a b
  where
    v1 = eval env e1; v2 = eval env e2

eval env (Or  e1 e2) = VB $ (||) (valB (eval env e1)) (valB (eval env e2))
eval env (And e1 e2) = VB $ (&&) (valB (eval env e1)) (valB (eval env e2))

eval env (Sconcat e1 e2) = VS $ (++) (valS (eval env e1)) (valS (eval env e2))

eval env (TakeN e1 e2) = VS $ take (fromInteger $ valI (eval env e1)) (valS (eval env e2))
eval env (DropN e1 e2) = VS $ drop (fromInteger $ valI (eval env e1)) (valS (eval env e2))

eval env (App e1 e2) = ($) (valF $ eval env e1) (eval env e2)

eval env (If e1 e2 e3) | v1 = eval env e2 | otherwise = eval env e3 where VB v1 = eval env e1

eval env (Lam n body) = VF $ \e -> let env' = bind env n e in eval env' body

eval env (Var n) = resolve env n

eval _ _ = error "unimplemented"
