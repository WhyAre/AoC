module Main where

import qualified Data.List as L
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Set

dijkstra :: M.Map String (L.List String) -> ST
main :: IO ()
main = do
  content <-
    L.foldl (\acc (src, dsts) -> M.insert src dsts acc) M.empty
      . L.map parse
      . lines
      <$> readFile "inputs/day11-sample.txt"
  print content
  where
    parse input =
      case splitOn ": " input of
        [src, dst] -> (src, words dst)
        _ -> error "idk"
