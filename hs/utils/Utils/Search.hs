module Utils.Search
  ( dfs, dfsOn
  , bfs, bfsOn
  , AStep(..)
  , astar, astarN, astarOn, astarOnN
  ) where

import Utils.Deque  qualified as D
import Utils.PQueue qualified as PQ

import Data.Set    qualified as S
import Data.IntSet qualified as IS

import Data.Foldable (foldl')

{-# INLINE dfs #-}
dfs :: Ord a => (a -> [a]) -> a -> [a]
dfs next start = dfsOn id next start

{-# INLINE[0] dfsOn #-}
dfsOn :: Ord r => (a -> r) -> (a -> [a]) -> a -> [a]
dfsOn repr next start = loop S.empty [start]
  where
    loop _ [] = []
    loop seen (x:xs)
      | r `S.member` seen = loop seen xs
      | otherwise         = x : loop seen' (next x ++ xs)
      where
        r = repr x
        seen' = S.insert r seen

{-# RULES "dfsOn/Int" dfsOn = dfsOnInt #-}
{-# INLINE dfsOnInt #-}
dfsOnInt :: (a -> Int) -> (a -> [a]) -> a -> [a]
dfsOnInt repr next start = loop IS.empty [start]
  where
    loop _ [] = []
    loop seen (x:xs)
      | r `IS.member` seen = loop seen xs
      | otherwise          = x : loop seen' (next x ++ xs)
      where
        r = repr x
        seen' = IS.insert r seen

{-# INLINE bfs #-}
bfs :: Ord a => (a -> [a]) -> a -> [a]
bfs next start = bfsOn id next [start]

{-# INLINE[0] bfsOn #-}
bfsOn :: Ord r => (a -> r) -> (a -> [a]) -> [a] -> [a]
bfsOn repr next starts = loop S.empty (D.fromList starts)
  where
    loop _ D.Empty = []
    loop seen (x D.:<| xs)
      | r `S.member` seen = loop seen xs
      | otherwise         = x : loop seen' (D.appendList nexts xs)
      where
        r = repr x
        seen' = S.insert r seen
        nexts = next x

{-# RULES "bfsOn/Int" bfsOn = bfsOnInt #-}
{-# INLINE bfsOnInt #-}
bfsOnInt :: (a -> Int) -> (a -> [a]) -> [a] -> [a]
bfsOnInt repr next starts = loop IS.empty (D.fromList starts)
  where
    loop _ D.Empty = []
    loop seen (x D.:<| xs)
      | r `IS.member` seen = loop seen xs
      | otherwise          = x : loop seen' (D.appendList nexts xs)
      where
        r = repr x
        seen' = IS.insert r seen
        nexts = next x

astar :: Ord a => (a -> [AStep a]) -> a -> [(a,Int)]
astar = astarOn id
{-# INLINE astar #-}

astarN :: Ord a => (a -> [AStep a]) -> [a] -> [(a,Int)]
astarN = astarOnN id
{-# INLINE astarN #-}

astarOn ::
  Ord b =>
  (a -> b)         {- ^ state characterization                                   -} ->
  (a -> [AStep a]) {- ^ step function (new state, step cost, distance heuristic) -} ->
  a                {- ^ starting state                                           -} ->
  [(a,Int)]        {- ^ list of states visited                                   -}
astarOn rep nexts start = astarOnN rep nexts [start]

astarOnN ::
  Ord b =>
  (a -> b)         {- ^ state characterization                                   -} ->
  (a -> [AStep a]) {- ^ step function (new state, step cost, distance heuristic) -} ->
  [a]              {- ^ starting states                                          -} ->
  [(a,Int)]        {- ^ list of states visited                                   -}
astarOnN rep nexts starts = go S.empty (PQ.fromList [(0, WC 0 s) | s <- starts])
  where
    go !seen = \case
      PQ.Empty -> []
      WC cost x PQ.:<| work
        | S.member r seen -> go seen work
        | otherwise         -> (x,cost) : go seen' work'
        where
          r     = rep x
          seen' = S.insert r seen
          work' = foldl' addWork work (nexts x)
          addWork w (AStep x' stepcost heuristic) =
            PQ.insert (cost' + heuristic) (WC cost' x') w
            where
              cost' = cost + stepcost
{-# INLINE astarOn #-}

data WithCost a = WC !Int a

data AStep a = AStep {
  astepNext      :: a,    -- ^ successor node
  astepCost      :: !Int, -- ^ cost of edge
  astepHeuristic :: !Int  -- ^ heuristic cost to goal from this new node
  } deriving Show
