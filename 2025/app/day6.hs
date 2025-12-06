module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Ord
import Debug.Trace
import Text.Printf

part1 :: [[String]] -> [Char] -> Int
part1 nums =
  doMath processedNums
  where
    processedNums = traceShowId $ map (map (read . stripChar '0')) nums

part2 :: [[String]] -> [Char] -> Int
part2 nums =
  doMath processedNums
  where
    processedNums = traceShowId $ map (map (read . stripChar '0') . transpose) nums

-- Removes char from front and back of string
stripChar :: (Eq a) => a -> [a] -> [a]
stripChar c =
  reverse . dropWhile (== c) . reverse . dropWhile (== c)

doMath :: [[Int]] -> [Char] -> Int
doMath nums ops =
  sum $ compute <$> zip nums ops
  where
    compute :: ([Int], Char) -> Int
    compute (ns, '+') = sum ns
    compute (ns, '*') = product ns
    compute (_, c) = error $ "dafuq is this: " ++ show c

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
        nums = traceShowId $ transpose $ words <$> replaceWithZeros firstRows
        ops = traceShowId $ head <$> words lastRow

        -- Replaces fake spaces with zeros
        replaceWithZeros list =
          transpose $ map replaceRow (transpose list)
          where
            replaceRow row =
              if all isSpace row
                then row
                else replace ' ' '0' row

        -- Replace a with b in list
        replace a b =
          map (\x -> if x == a then b else x)
