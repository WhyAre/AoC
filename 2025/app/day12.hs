module Main where

-- T for Tree

import qualified Control.Monad as S
import Data.Bits (shiftL, xor, (.|.))
import Data.Char (digitToInt)
import qualified Data.List as L
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import qualified Data.Sequence as S
import qualified Data.Set as Data
import qualified Data.Set as T
import Debug.Trace (traceShow, traceShowId)
import Text.Printf (printf)

bfs :: Int -> [Int] -> Int
bfs endState ops =
  traceShow "New" $
    go (S.singleton (0, 0)) (T.singleton 0)
  where
    go :: S.Seq (Int, Int) -> Data.Set Int -> Int
    go S.Empty _ = error "gg"
    go ((depth, curState) S.:<| rest) visited
      | curState == endState = depth
      | otherwise =
          traceShow ("Curr State: " ++ show curState) $
            let (newQueue, newVisited) = foldl step (rest, visited) nextStates
             in go newQueue newVisited
      where
        nextStates = L.filter (`T.notMember` visited) . L.map (xor curState) $ ops
        step (q, v) s =
          traceShow
            ("Next: " ++ show s)
            (q S.|> (depth + 1, s), T.insert s v)

part1 :: [(Int, [Int])] -> Int
part1 = L.sum . L.map (uncurry bfs)

main :: IO ()
main = do
  list <- L.map parseLine . lines <$> readFile "inputs/day10.txt"
  let ans1 = part1 list
   in printf "Part 1: %d\n" ans1
  where
    parseLine :: String -> (Int, [Int])
    parseLine row =
      (endState, ops)
      where
        (endState0, ops0) = case parts of
          a : b -> (removeSurroundingBrace a, L.map removeSurroundingBrace (init b))
          _ -> error "What"
        endState = parseEndState endState0
        ops = L.map parseOps ops0
        parts = words row

parseEndState :: String -> Int
parseEndState [] = 0
parseEndState (x : xs)
  | x == '#' = (parseEndState xs `shiftL` 1) + 1
  | otherwise = parseEndState xs `shiftL` 1

-- "1,2,3" -> 14
parseOps :: String -> Int
parseOps =
  foldl (.|.) 0 . L.map (computeMask . read) . splitOn ","
  where
    computeMask = shiftL 1

removeSurroundingBrace :: String -> String
removeSurroundingBrace = init . drop 1