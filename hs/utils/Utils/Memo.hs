module Utils.Memo
  ( HasTrie
  , memo, memo2, memo3, memo4, memo5, memo6
  ) where

import Data.MemoTrie (HasTrie, memo, memo2, memo3, mup)

memo4 ::
  (HasTrie a, HasTrie b, HasTrie c, HasTrie d) =>
  (a -> b -> c -> d -> e) ->
  (a -> b -> c -> d -> e)
memo4 = mup memo3

memo5 ::
  (HasTrie a, HasTrie b, HasTrie c, HasTrie d, HasTrie e) =>
  (a -> b -> c -> d -> e -> f) ->
  (a -> b -> c -> d -> e -> f)
memo5 = mup memo4

memo6 ::
  (HasTrie a, HasTrie b, HasTrie c, HasTrie d, HasTrie e, HasTrie f) =>
  (a -> b -> c -> d -> e -> f -> g) ->
  (a -> b -> c -> d -> e -> f -> g)
memo6 = mup memo5
