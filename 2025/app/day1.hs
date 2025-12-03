module Main where

import Data.List
import Text.Printf (printf)

dialSize :: Integer
dialSize = 100

startPos :: Integer
startPos = 50

parseLine :: String -> Integer
parseLine (x : xs) =
  case x of
    'L' -> -num
    'R' -> num
    others -> error ("Fail to parse: " ++ show others)
  where
    num = read xs
parseLine _ = error "Fail to parse"

part1 :: [Integer] -> Int
part1 list =
  length $ filter (0 ==) $ scanl (\acc cur -> (acc + cur) `mod` dialSize) start list
  where
    start = 50

-- How many times we land on 0 while rotating from `pos` by `rot` clicks
countZeroHits :: Integer -> Integer -> Integer
countZeroHits pos rot
  | rot == 0 = 0
  | otherwise =
      let s = if rot > 0 then 1 else -1 -- direction: R = +1, L = -1
          steps = abs rot -- how many clicks
          m = dialSize
          k0 = (-s * pos) `mod` m -- solution to pos + s*k ≡ 0 (mod m)
          firstK = if k0 == 0 then m else k0 -- smallest positive k hitting 0
       in if steps < firstK
            then 0
            else 1 + (steps - firstK) `div` m -- plus full extra loops

-- state = (currentPosition, totalTimesAtZeroSoFar)
stepDial :: (Integer, Integer) -> Integer -> (Integer, Integer)
stepDial (pos, acc) rot =
  let hits = countZeroHits pos rot
      newPos = (pos + rot) `mod` dialSize
   in (newPos, acc + hits)

-- Part 2: count *all* times we click onto 0 (during + at end of rotations)
part2 :: [Integer] -> Integer
part2 rotations =
  snd $ foldl' stepDial (startPos, 0) rotations

main :: IO ()
main = do
  raw <- readFile "inputs/day1.txt"
  let content = map parseLine (lines raw)
      ans1 = part1 content
  printf "Part 1: %d\n" ans1
  let ans2 = part2 content
  printf "Part 2: %d\n" ans2