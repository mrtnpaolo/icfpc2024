module Utils.Input
  ( getInput
  , getInputLines
  , getInputArray
  , getInputMap
  , getCleanInput
  ) where

import Utils.Coord

import System.Environment (getArgs)
import Text.Printf (printf)
import Data.Array.Unboxed qualified as A
import Data.Map.Strict qualified as M

getInput :: (String -> a) -> String -> Int -> IO a
getInput parse fmt n =
  do args <- getArgs
     parse <$> case args of
       []    -> readFile (printf fmt n)
       "-":_ -> getContents
       fn:_  -> readFile fn

getInputLines :: (String -> a) -> String -> Int -> IO [a]
getInputLines parse fmt n = getInput (map parse . lines) fmt n

getInputArray :: String -> Int -> IO (A.UArray Coord Char)
getInputArray fmt n = makeArray <$> getInputLines id fmt n
  where
    makeArray rows =
      A.listArray bounds (concat rows)
        where
          height = length rows
          width  = length (head rows)
          bounds = ( origin, C (height-1) (width-1) )

getInputMap :: String -> Int -> IO (M.Map Coord Char)
getInputMap = getInput (M.fromList . withCoords id . lines)

getCleanInput :: String -> (String -> a) -> String -> Int -> IO a
getCleanInput without parse = getInput (parse . map clean)
  where
    clean c
      | c `notElem` without = c
      | otherwise = ' '
