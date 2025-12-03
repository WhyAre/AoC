module Main where

import Control.Exception (assert)
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Debug.Trace
import Text.Printf (printf)

-- Definition of invalid ID in part 1
isInvalid1 :: Integer -> Bool
isInvalid1 n =
  let s = show n
      len = length s
      half = len `div` 2
      (a, b) = splitAt half s
   in assert (even len) $
        a == b

-- Definition of invalid ID in part 2
isInvalid2 :: Integer -> Bool
isInvalid2 n =
  s `isInfixOf` ss
  where
    s = show n
    ss = tail $ init $ s ++ s

part1 :: [(Integer, Integer)] -> Integer
part1 ranges =
  sum invalidIDs
  where
    invalidIDs =
      ranges >>= \(start, end) ->
        filter isInvalid1 [start .. end]

part2 :: [(Integer, Integer)] -> Integer
part2 ranges =
  sum invalidIDs
  where
    invalidIDs =
      traceShowId $
        ranges >>= \(start, end) ->
          filter isInvalid2 [start .. end]

parseRange :: [Char] -> (Integer, Integer)
parseRange input =
  (a, b)
  where
    (a, b) = case splitOn "-" input of
      [a0, b0] -> (read a0, read b0)
      parts ->
        error ("parseRange: expected exactly two numbers, got: " ++ show parts)

main :: IO ()
main = do
  content <- readFile "inputs/day2-sample.txt"
  let ranges = map parseRange $ splitOn "," content
      ans = part1 ranges
  printf "Part 1: %d\n" ans
  let ans2 = part2 ranges
  printf "Part 2: %d\n" ans2