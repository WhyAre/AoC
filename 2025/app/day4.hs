module Main where

import Control.Monad (filterM)
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import qualified Data.Bifunctor
import Data.Bits (Bits (xor))
import Debug.Trace
import Text.Printf

dirs :: [(Int, Int)]
dirs = [(r, c) | r <- [-1 .. 1], c <- [-1 .. 1], (r, c) /= (0, 0)]

countNumSurroundingRolls :: Array (Int, Int) Char -> (Int, Int) -> Int
countNumSurroundingRolls board (r, c) =
  length $
    filter (\x -> board ! x == '@') $
      filter (inRange (bounds board)) $
        map (Data.Bifunctor.bimap (r +) (c +)) dirs

part1 :: Array (Int, Int) Char -> Int
part1 board =
  length $
    filter (\x -> countNumSurroundingRolls board x < 4) $
      filter (\x -> board ! x == '@') (indices board)

removeRolls :: Array (Int, Int) Char -> (Int, Array (Int, Int) Char)
removeRolls board =
  (numRemoved, board // [(x, '.') | x <- indicesToRemove])
  where
    indicesToRemove =
      filter (\x -> countNumSurroundingRolls board x < 4) $
        filter (\x -> board ! x == '@') (indices board)
    numRemoved = length indicesToRemove

part2 :: Array (Int, Int) Char -> Int
part2 =
  go 0
  where
    go acc board
      | numRemoved == 0 = acc
      | otherwise =
          go (acc + numRemoved) newBoard
      where
        (numRemoved, newBoard) = removeRolls board

toBoard :: [String] -> Array (Int, Int) Char
toBoard rows =
  array
    ((0, 0), (nRows - 1, nCols - 1))
    [ ((i, j), c)
      | (i, row) <- zip [0 ..] rows,
        (j, c) <- zip [0 ..] row
    ]
  where
    nRows = length rows
    nCols = length (head rows)

main :: IO ()
main = do
  ls <- lines <$> readFile "inputs/day4-sample.txt"
  let board = toBoard ls
      ans1 = part1 board
      ans2 = part2 board
  printf "Part 1: %d\n" ans1
  printf "Part 2: %d\n" ans2