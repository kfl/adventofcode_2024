module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Array.Unboxed as U
import Data.Array.Unboxed ((!), (!?))

test =  parse [ "89010123"
              , "78121874"
              , "87430965"
              , "96549874"
              , "45678903"
              , "32019012"
              , "01329801"
              , "10456732"
              ]
input = parse . lines <$> readFile "input.txt"

type Row = Int
type Col = Int
type Pos = (Col, Row)
type Dir = (Col, Row)

type Grid = U.UArray Pos Int
type Input = Grid

parse :: [String] -> Input
parse rows@(cols : _) = U.array bounds heights
  where heights = [ ((x,y), C.digitToInt c) | (y, row) <- zip [0..] rows
                                            , (x, c) <- zip [0..] row]
        bounds = ((0,0), (length cols - 1, length rows - 1))

directions :: [Dir]
directions@[north, south, east, west] = [ (0,-1), (0,1), (1,0), (-1,0) ]

(a, b) |+| (c, d) = (a+c, b+d)

follow next = L.foldl' go Set.empty
  where go seen x
          | x `Set.member` seen = seen
          | otherwise = L.foldl' go (Set.insert x seen) $ next x

score :: Grid -> Pos -> Int
score grid trailhead = Set.size $ Set.filter ((== 9) . (grid !)) $ follow next [trailhead]
  where next from = [ to | d <- directions, let to = from |+| d
                         , Just (h+1) == grid !? to ]
          where h = grid ! from

part1 :: Input -> Int
part1 input = sum [score input t | t <- trailheads]
  where trailheads = [ p | (p, 0) <- U.assocs input ]
answer1 = part1 <$> input

-- `acc` is a multi-map of all possible current locations and
-- multiplicity of different paths leading to that location, `hn` is
-- the next height take. Returns multi-map of all possible next
-- locations and their multiplicity.
multiStep :: Grid -> Map Pos Int -> Int -> Map Pos Int
multiStep grid acc hn = Map.fromListWith (+) [(to, n) | (from, n) <- Map.assocs acc
                                                      , d <- directions, let to = from |+| d
                                                      , Just hn == grid !? to ]

rating :: Grid -> Pos -> Int
rating grid trailhead = sum $ L.foldl' (multiStep grid) (Map.singleton trailhead 1) [1..9]

part2 :: Input -> Int
part2 input = sum [rating input t | t <- trailheads]
  where trailheads = [ p | (p, 0) <- U.assocs input ]
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
