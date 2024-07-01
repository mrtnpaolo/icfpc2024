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
-- e = [| "echo " ++ (drop (stoi "g") (itos 379390335319035451457)) |]

-- f = [| \f -> \n -> if (E.==) n 0 then 1 else (E.*) n (f ((E.-) n 1)) |] :: Q Exp
-- e = [| $y $f 10 |]

rep = [| \s -> \f -> \n -> if n == 1 then s else (++) s (f (n - 1)) |] :: Q Exp
two = [| ($y ($rep "R") 49) ++ "D" ++ ($y ($rep "L") 49) ++ "D" |]
--e = [| "solve lambdaman9 " ++ $y ($rep $two) 25 |]

l8 = [| ($y ($rep "R") 100) ++ ($y ($rep "D") 100) ++ ($y ($rep "L") 100) ++ ($y ($rep "U") 100) |]
e = [| "solve lambdaman9 " ++ $y ($rep $l8) 100 |]
