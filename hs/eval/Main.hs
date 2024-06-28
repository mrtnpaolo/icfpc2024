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
    -- print ts
    let (ps,_) = parse ts
    print ps
    putStrLn $ case (eval emptyEnv ps) of
      VS s -> decode94 s
      x -> val x

getInput =
  do args <- getArgs
     case args of
       "-":_ -> getContents
       fn:_  -> readFile fn

getInputs = lines <$> getInput
