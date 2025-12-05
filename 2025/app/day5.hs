module Main where

import Data.List
import Data.List.Split
import Data.Ord
import Text.Printf

compressList :: [(Int, Int)] -> [(Int, Int)]
compressList list =
  aux [] (sortBy (comparing fst) list)
  where
    aux acc [] = acc
    aux [] ((start, end) : xs) = aux [(start, end)] xs
    aux ((top_start, top_end) : ys) ((start, end) : xs) =
      if start > top_end
        then
          aux ((start, end) : (top_start, top_end) : ys) xs
        else
          aux ((top_start, max end top_end) : ys) xs

-- Sort ascending start O(n log n)
-- greedy merge O(n)
-- count ranges
part2 :: [(Int, Int)] -> Int
part2 freshIds =
  sum $ map (\(start, end) -> end - start + 1) processedList
  where
    processedList :: [(Int, Int)]
    processedList = compressList freshIds

part1 :: [(Int, Int)] -> [Int] -> Int
part1 freshIds availableIds =
  length $ filter isFresh availableIds
  where
    isFresh e =
      any (\(start, end) -> start <= e && e <= end) freshIds

main :: IO ()
main = do
  content0 <- readFile "inputs/day5-sample.txt"
  let (freshIds0, availableIds0) =
        case splitOn "\n\n" content0 of
          [a, b] -> (a, b)
          c -> error $ "fail to parse" ++ show c
      freshIds = map parseRange (lines freshIds0)
      availableIds = map read (lines availableIds0)
      ans1 = part1 freshIds availableIds
      ans2 = part2 freshIds
  printf "Part 1: %d\n" ans1
  printf "Part 1: %d\n" ans2
  where
    parseRange line =
      let (start, end) =
            case splitOn "-" line of
              [a, b] -> (a, b)
              c -> error $ "fail to parse" ++ show c
       in (read start, read end)