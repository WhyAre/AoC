module Main where

import Data.DisjointSet (DisjointSet, empty, equivalent, fromLists, sets, toSets, union)
import Data.List (sortBy, tails)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Text.Printf (printf)

type Coord = (Int, Int, Int)

distBetween :: Coord -> Coord -> Int
distBetween (x1, y1, z1) (x2, y2, z2) =
  (x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int) + (z2 - z1) ^ (2 :: Int)

createPairs :: [Coord] -> [(Int, (Coord, Coord))]
createPairs coords =
  sortBy (comparing fst) pairs
  where
    pairs = [(distBetween x y, (x, y)) | (x : ys) <- tails coords, y <- ys]

part1 :: [Coord] -> Int -> Int
part1 coords numPairs =
  product $ take 3 $ sortBy (flip compare) $ map length (toSets disjointSets)
  where
    disjointSets = getDisjointSets (createPairs coords) numPairs empty

part2 :: [Coord] -> Int
part2 coords =
  (\((x1, _, _), (x2, _, _)) -> x1 * x2) $ head mergedCoords
  where
    mergedCoords = kruskal (createPairs coords) (fromLists (map pure coords)) []

getDisjointSets :: [(Int, (Coord, Coord))] -> Int -> DisjointSet Coord -> DisjointSet Coord
getDisjointSets pairs numPairsNeeded ufds
  | numPairsNeeded <= 0 = ufds
  | otherwise =
      -- traceShow ("Merging " ++ show (head pairs)) $
      getDisjointSets remainingPairs (numPairsNeeded - 1) newUfds
  where
    getNextPair :: [(Int, (Coord, Coord))] -> ([(Int, (Coord, Coord))], (Coord, Coord))
    getNextPair [] = error "No more pairs"
    getNextPair ((_, (c1, c2)) : xs) = (xs, (c1, c2))
    (remainingPairs, nextPair) = getNextPair pairs
    (nextC1, nextC2) = nextPair
    newUfds = union nextC1 nextC2 ufds

kruskal :: [(Int, (Coord, Coord))] -> DisjointSet Coord -> [(Coord, Coord)] -> [(Coord, Coord)]
kruskal pairs ufds mergedPairs
  | sets ufds <= 1 = mergedPairs
  | otherwise =
      -- traceShow ("Merging " ++ show (head pairs)) $
      kruskal remainingPairs newUfds (nextPair : mergedPairs)
  where
    getNextPair :: [(Int, (Coord, Coord))] -> DisjointSet Coord -> ([(Int, (Coord, Coord))], (Coord, Coord))
    getNextPair [] _ = error "No more pairs"
    getNextPair ((_, (c1, c2)) : xs) existingSets =
      if not $ equivalent c1 c2 existingSets
        then (xs, (c1, c2))
        else getNextPair xs existingSets
    (remainingPairs, nextPair) = getNextPair pairs ufds
    (nextC1, nextC2) = nextPair
    newUfds = union nextC1 nextC2 ufds

main :: IO ()
main = do
  coords <- map lineToCoord . lines <$> readFile "inputs/day8.txt"
  let ans1 = part1 coords 1000
      ans2 = part2 coords
  printf "Part 1: %d\n" ans1
  printf "Part 2: %d\n" ans2
  where
    lineToCoord :: String -> Coord
    lineToCoord row =
      case splitOn "," row of
        [a, b, c] -> (read a, read b, read c)
        c -> error $ "wtf is " ++ show c