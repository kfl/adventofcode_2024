module Main where

import qualified Data.Char as C
import qualified Data.List as L

import qualified Data.Set as Set

import qualified Data.Array.Unboxed as U
import Data.Array.Unboxed ((!), (!?))
import qualified Data.HashMap.Strict as HMap

import Data.Maybe (maybeToList)

import qualified Data.Sequence as Seq
import Data.Sequence (Seq (..), (><))



test =  parse [ "###############"
              , "#...#...#.....#"
              , "#.#.#.#.#.###.#"
              , "#S#...#.#.#...#"
              , "#######.#.#.###"
              , "#######.#.#...#"
              , "#######.#.###.#"
              , "###..E#...#...#"
              , "###.#######.###"
              , "#...###...#...#"
              , "#.#####.#.###.#"
              , "#.#...#.#.#...#"
              , "#.#.#.#.#.#.###"
              , "#...#...#...###"
              , "###############"
              ]
input = parse . lines <$> readFile "input.txt"

type Row = Int
type Col = Int
type Pos = (Col, Row)
type Dir = (Col, Row)

type Grid = U.UArray Pos Char
type Input = Grid

parse :: [String] -> Input
parse rows@(cols : _) = U.array bounds letters
  where letters = [ ((x,y), c) | (y, row) <- zip [0..] rows
                               , (x, c) <- zip [0..] row]
        bounds = ((0,0), (length cols - 1, length rows - 1))

directions :: [Dir]
directions@[north, south, east, west] = [ (0,-1), (0,1), (1,0), (-1,0) ]

(a, b) |+| (c, d) = (a+c, b+d)

type Distance = Int

bfs :: Ord a => (a -> [a]) -> a -> (a -> Bool) -> Maybe [(a, Distance)]
bfs next initial found = (`zip` [0..]) <$> go Set.empty (Seq.singleton [initial])
  where
    go _ Empty = Nothing
    go seen (path@(x:_) :<| queue)
      | found x             = Just $ reverse path
      | x `Set.member` seen = go seen queue
      | otherwise           = go (Set.insert x seen) (queue <> Seq.fromList [ y : path | y <- next x])


part1 :: Distance -> Input -> Int
part1 threshold grid = length cheats
  where start : _ = [ p | (p, 'S') <- U.assocs grid]
        goal : _  = [ p | (p, 'E') <- U.assocs grid]
        found p = p == goal
        next from = [ to | d <- directions
                         , let to = from |+| d
                         , maybe False (/= '#') (grid !? to)]
        Just path = bfs next start found
        distMap = HMap.fromList path
        possible from = [ to | d <- directions, let to = from |+| d
                             , U.bounds grid `U.inRange` to ]
        cheats    = [ (p1, p2, r) | (p1, c1) <- path
                                  , p2 <- possible =<< possible p1
                                  , c2 <- maybeToList (distMap HMap.!? p2)
                                  , let r = c2 - c1 - 2
                                  , r >= threshold ]

answer1 = part1 100 <$> input


part2 :: Distance -> Input -> Int
part2 threshold grid = length cheats
  where start : _ = [ p | (p, 'S') <- U.assocs grid]
        goal : _  = [ p | (p, 'E') <- U.assocs grid]
        found p = p == goal
        next from = [ to | d <- directions
                         , let to = from |+| d
                         , maybe False (/= '#') (grid !? to)]
        Just path = bfs next start found
        distMap = HMap.fromList path
        ((minX, minY), (maxX, maxY)) = U.bounds grid
        cheats    = [ (p1, p2, r) | (p1@(x1,y1), c1) <- path
                                  , y2 <- [max minY (y1 - 20) .. min maxY (y1 + 20)]
                                  , let dy = abs $ y1 - y2
                                  , x2 <- [max minX (x1 - 20 + dy) .. min maxX (x1 + 20 - dy)]
                                  , let dx = abs $ x1 - x2
                                        d  = dx + dy
                                        p2 = (x2,y2)
                                  , c2 <- maybeToList (distMap HMap.!? p2)
                                  , let r = c2 - c1 - d
                                  , r >= threshold ]
answer2 = part2 100 <$> input

main = do
  inp <- input
  print $ part1 100 inp
  print $ part2 100 inp
