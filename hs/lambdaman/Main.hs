module Main (main) where

import Utils

import Data.Ix
import Data.Array.Unboxed qualified as A
import Data.Map.Strict    qualified as M

main =
  do inp <- getInputArray "../puzzles/lambdaman/lambdaman%d.txt" 1
     printCave inp
     let [start] = [ c | (c,'L') <- A.assocs inp ]
     print start
     -- printCave `mapM_` solve inp start
     putStrLn $ head (solve inp start)
  where
    printCave = putStrLn . drawCoords . M.fromList . A.assocs

visit a c = a A.// [(c,' ')]

{-
  Ord b =>
  (a -> b)         {- ^ state characterization                                   -} ->
  (a -> [AStep a]) {- ^ step function (new state, step cost, distance heuristic) -} ->
  a                {- ^ starting state                                           -} ->
  [(a,Int)]        {- ^ list of states visited                                   -}
astarOn rep nexts start = astarOnN rep nexts [start]

bfsOn :: Ord r => (a -> r) -> (a -> [a]) -> [a] -> [a]
bfsOn repr next starts = loop S.empty (D.fromList starts)
-}

solve a start = [ reverse path | (_,a,path) <- visits, swooped a ]
  where
    swooped = null . filter ('.'==) . A.elems
    visits = bfsOn repr next [(start,visit a start,[])]
    repr (c,a,_) = (c,a)
    next (c,a,p)
      | ('.'==) `any` (A.elems a) = nexts
      | otherwise = []
      where
        nexts =
          [ (d,visit a d,dir:p)
          | (dir,d) <- zip "URDL" (cardinal c), inside d, (a A.! d) `elem` (". " :: String) ]
    inside = inRange (A.bounds a)
