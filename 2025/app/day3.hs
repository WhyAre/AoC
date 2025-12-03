module Main where

import Data.List
import Data.Ord
import Debug.Trace
import Text.Printf

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start) . drop start

-- Given a string of numbers,
-- return the index and value of highest digit
getHighestDigit :: String -> (Int, Char)
getHighestDigit input =
  maximumBy (comparing snd <> flip (comparing fst)) list
  where
    list = [(i, x) | (i, x) <- zip [0 ..] input]

getHighestNumber :: Int -> String -> Int
getHighestNumber len input =
  read $ aux len input
  where
    aux :: Int -> String -> String
    aux n s
      | n <= 0 = ""
      | otherwise =
          maxDigit : aux (n - 1) (drop (maxDigitIdx + 1) s)
      where
        strLen = length s
        (maxDigitIdx, maxDigit) = getHighestDigit $ take (strLen - n + 1) s

part1 :: [String] -> Int
part1 list =
  sum $ map (traceShowId . getHighestNumber 2) list

part2 :: [String] -> Int
part2 list =
  sum $ map (traceShowId . getHighestNumber 12) list

main :: IO ()
main = do
  content <- readFile "inputs/day3.txt"
  let parts = lines content
      ans1 = part1 parts
  printf "Part 1: %d\n" ans1
  let ans2 = part2 parts
  printf "Part 2: %d\n" ans2