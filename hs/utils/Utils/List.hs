module Utils.List
  ( count
  , freqs
  , findRepeatOn
  ) where

import Data.List (foldl')
import Data.Map.Strict qualified as M (toAscList,fromListWith,empty,(!?),insert)

count :: (a -> Bool) -> [a] -> Int
count p xs = foldl' f 0 xs
  where
    f n x | p x = n+1 | otherwise = n

freqs :: (Ord a) => [a] -> [(a,Int)]
freqs xs = combine [ (x,1) | x <- xs ]
  where
    combine = M.toAscList . M.fromListWith (+)

findRepeatOn f = go M.empty 0
  where
    go seen n (x:xs) =
      case seen M.!? f x of
        Nothing -> go (M.insert (f x) n seen) (n+1) xs
        Just m  -> (m,n)
