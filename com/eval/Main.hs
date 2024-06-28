module Main (main) where

import ICFP

import Data.Foldable

import System.Environment (getArgs)

main =
 do inp <- getInputs
    forM_ inp $ \x ->
      do f x
         putStrLn ""

f inp =
 do putStrLn inp
    let ts = tokenize inp
    print ts
    let (ps,_) = parse ts
    print ps
    let e = val (eval ps)
    putStrLn e

getInput =
  do args <- getArgs
     case args of
       "-":_ -> getContents
       fn:_  -> readFile fn

getInputs = lines <$> getInput
