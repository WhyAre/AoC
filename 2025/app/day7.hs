module Main where

import Data.Array
import Data.List
import Data.Maybe
import Data.Set
import Debug.Trace

dfs :: (Int, Int) -> Array (Int, Int) Char -> Set (Int, Int) -> (Int, Set (Int, Int))
dfs (r, c) grid visited =
  traceShow ("Coord", r, c, "hasSplit", hasSplit) $
    Data.List.foldl
      ( \(res, v) (nr, nc) ->
          let (newRes, newV) = dfs (nr, nc) grid (Data.Set.insert (nr, nc) v)
           in (newRes + res, newV)
      )
      (if hasSplit then 1 else 0, visited)
      nextCoords
  where
    hasSplit =
      let bottom = (r + 1, c)
       in inRange (bounds grid) bottom && grid ! bottom == '^'
    nextCoords =
      traceShowId $
        Data.List.filter (`notElem` visited) $
          Data.List.filter (inRange (bounds grid)) $
            if not hasSplit
              then [(r + 1, c)]
              else [(r + 1, c + 1), (r + 1, c - 1)]

main :: IO ()
main = do
  (start, grid) <- parse <$> readFile "inputs/day7.txt"
  let (ans1, _) = dfs start grid empty
  print grid
  print ans1
  where
    parse :: String -> ((Int, Int), Array (Int, Int) Char)
    parse content =
      (start, arr)
      where
        rows = traceShowId $ length $ lines content
        cols = traceShowId $ length $ head $ lines content
        start = fst $ fromJust $ find (('S' ==) . snd) (zip (indices arr) (Data.Array.elems arr))
        arr =
          listArray
            ((0, 0), (rows - 1, cols - 1))
            (concat $ lines content)