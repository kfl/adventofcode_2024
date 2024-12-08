module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (fromMaybe)
import Data.Ix (inRange)


test =  parse [ "............"
              , "........0..."
              , ".....0......"
              , ".......0...."
              , "....0......."
              , "......A....."
              , "............"
              , "............"
              , "........A..."
              , ".........A.."
              , "............"
              , "............"
              ]
input = parse . lines <$> readFile "input.txt"

type Row = Int
type Col = Int
type Pos = (Col, Row)
type Input = ((Int, Int), [(Pos, Char)])

parse :: [String] -> Input
parse rows@(cols: _) = ((length cols - 1, length rows - 1),
                        [ ((x,y), c) | (y, row) <- zip [0..] rows
                                     , (x, c) <- zip [0..] row
                                     , C.isAlphaNum c])

freqMap :: [(Pos, Char)] -> Map Char [Pos]
freqMap antennas = L.foldl' upsert Map.empty antennas
  where upsert acc (pos, c) =
          Map.alter (Just . (pos:) . fromMaybe []) c acc

allPairs keys = [ (k1, k2) | k1 : ks <- L.tails keys, k2 <- ks]

uniqueAntinodes anti (upper, antennas) = Set.fromList antis
  where freqs = Map.elems $ freqMap antennas
        inBounds = inRange ((0,0), upper)
        antis = freqs >>= allPairs >>= anti inBounds

antinodes inBounds ((a,b), (c,d)) = filter inBounds [(2*a-c, 2*b-d), (2*c-a, 2*d-b)]

part1 :: Input -> Int
part1 input = Set.size $ uniqueAntinodes antinodes input

answer1 = part1 <$> input


antilines :: (Pos -> Bool) -> (Pos, Pos) -> [Pos]
antilines inBounds (p1, p2) = line p1 p2 ++ line p2 p1
  where step x y i = (i+1)*x - i*y
        line (a,b) (c,d) = takeWhile inBounds [ (step a c i, step b d i) | i <- [0..] ]

part2 :: Input -> Int
part2 input = Set.size $ uniqueAntinodes antilines input

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
