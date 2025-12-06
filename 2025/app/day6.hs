module Main where

import Data.Char
import Data.List
import Data.List.Split (splitWhen)
import Debug.Trace
import Text.Printf

part1 :: [[String]] -> [Char] -> Int
part1 nums ops =
  sum $
    map (uncurry solveSingleProblem) $
      traceShowId $
        zipWith (\n op -> (map read n, op)) nums ops

part2 :: [[String]] -> [Char] -> Int
part2 nums ops =
  sum $
    map (uncurry solveSingleProblem) $
      traceShowId $
        zipWith (\n op -> (map read (transpose n), op)) nums ops

solveSingleProblem :: [Int] -> Char -> Int
solveSingleProblem ns '+' = sum ns
solveSingleProblem ns '*' = product ns
solveSingleProblem _ c = error $ "dafuq is this operator: " ++ show c

main :: IO ()
main = do
  (nums, ops) <- parseInput <$> readFile "inputs/day6-sample.txt"
  let ans1 = part1 nums ops
      ans2 = part2 nums ops
  printf "Part 1: %d\n" ans1
  printf "Part 2: %d\n" ans2
  where
    parseInput :: String -> ([[String]], [Char])
    parseInput content =
      (nums, ops)
      where
        (firstRows, lastRow) = (init (lines content), last (lines content))
        nums = traceShowId $ transpose <$> splitWhen (all isSpace) (transpose firstRows)
        ops = traceShowId $ head <$> words lastRow