module Main where

import Data.List (maximumBy, tails)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Debug.Trace (traceShowId)
import Text.Printf (printf)

type Coord = (Int, Int)

computeArea :: Coord -> Coord -> Int
computeArea (x1, y1) (x2, y2) =
  (1 + abs (y2 - y1)) * (1 + abs (x2 - x1))

part1 :: [Coord] -> Int
part1 coords =
  fst $ maximumBy (comparing fst) pairs
  where
    pairs = [(computeArea x y, (x, y)) | (x : ys) <- tails coords, y <- ys]

main :: IO ()
main = do
  coords <- map splitByComma . lines <$> readFile "inputs/day9.txt"
  let ans1 = part1 coords
  printf "Part 1: %d\n" ans1
  where
    splitByComma :: String -> (Int, Int)
    splitByComma row =
      case splitOn "," row of
        [a, b] -> (read a, read b)
        c -> error $ "wtf is " ++ show c