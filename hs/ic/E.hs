{-# LANGUAGE NoImplicitPrelude #-}
module E where

import ICFP.Base94

import Prelude qualified as P
import Language.Haskell.TH

data Bool = True | False
True = P.undefined
False = P.undefined

not, stoi, itos :: Q Exp -> Q Exp
not = P.undefined
stoi = P.undefined
itos = P.undefined

(+), (-), (*), (/), (%) :: Q Exp -> Q Exp
(+) = P.undefined
(-) = P.undefined
(*) = P.undefined
(/) = P.undefined
(%) = P.undefined

(<), (>), (==) :: Q Exp -> Q Exp
(<) = P.undefined
(>) = P.undefined
(==) = P.undefined

(||), (&&) :: Q Exp -> Q Exp
(||) = P.undefined
(&&) = P.undefined

(++) :: Q Exp -> Q Exp
(++) = P.undefined

take, drop :: Q Exp -> Q Exp -> Q Exp
take _ _ = P.undefined
drop _ _ = P.undefined

y, k, i :: Q Exp
y = [| (\f -> (\x -> f (x x)) (\x -> f (x x))) |]

fac = [| \f -> \n -> if (==) n 0 then 1 else (*) n (f ((-) n 1)) |] :: Q Exp

k = [| \x -> \y -> x |]
i = [| \x -> x |]

e :: Q Exp
-- e = [| "echo " ++ (itos 379390335319035451457) |]
e = [| "echo " ++ (drop (stoi "g") (itos 379390335319035451457)) |]
