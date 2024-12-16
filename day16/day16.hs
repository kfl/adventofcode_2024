{-# LANGUAGE LambdaCase, Strict, ViewPatterns #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Array.Unboxed as U
import Data.Array.Unboxed ((!))
import qualified Data.Hashable as H
import qualified Data.HashMap.Strict as Map
import qualified Data.HashPSQ as Q
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)


test =  parse [ "###############"
              , "#.......#....E#"
              , "#.#.###.#.###.#"
              , "#.....#.#...#.#"
              , "#.###.#####.#.#"
              , "#.#.#.......#.#"
              , "#.#.#####.###.#"
              , "#...........#.#"
              , "###.#.#####.#.#"
              , "#...#.....#.#.#"
              , "#.#.#.###.#.#.#"
              , "#.....#...#.#.#"
              , "#.###.#.#.#.#.#"
              , "#S..#.....#...#"
              , "###############"
              ]
test2 = parse [ "#################"
              , "#...#...#...#..E#"
              , "#.#.#.#.#.#.#.#.#"
              , "#.#.#.#...#...#.#"
              , "#.#.#.#.###.#.#.#"
              , "#...#.#.#.....#.#"
              , "#.#.#.#.#.#####.#"
              , "#.#...#.#.#.....#"
              , "#.#.#####.#.###.#"
              , "#.#.#.......#...#"
              , "#.#.###.#####.###"
              , "#.#.#...#.....#.#"
              , "#.#.#.#####.###.#"
              , "#.#.#.........#.#"
              , "#.#.#.#########.#"
              , "#S#.............#"
              , "#################"
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


type Direction = (Int, Int)
directions :: [Direction]
directions@[north, south, east, west] = [ (0,-1), (0,1), (1,0), (-1,0) ]

(a, b) |+| (c, d) = (a+c, b+d)


dijkstra :: (Num cost, Ord cost, Ord state, H.Hashable state)
         => (state -> [(state, cost)])
         -> (state -> Bool)
         -> state
         -> Maybe (cost, Map.HashMap state (cost, Set state))
dijkstra next found initial = loop initPathCost startFrontier
  where
    x `less` may = maybe True (x <=) may
    m !? state = fst <$> Map.lookup state m

    update n c queue = snd $ Q.alter upsert n queue
      where
        upsert Nothing = ((), Just(c, ()))
        upsert (Just(c', _)) = ((), Just(min c c', ()))

    updateCost n s c pathCost =
      Map.alter (\case Just (c', ss) | c == c' -> Just (c, Set.insert s ss)
                       _ -> Just (c, Set.singleton s)) n pathCost

    startFrontier = Q.singleton initial 0 ()
    initPathCost = Map.empty

    loop pathCost (Q.minView -> Nothing) = Nothing
    loop pathCost (Q.minView -> Just(s, c, _, frontier))
      | found s = Just (c, pathCost)
      | otherwise = loop pathCost' frontier'
      where
        relevant = [ (n, cc) | (n, sc) <- next s,
                               let cc = c + sc,
                               cc `less` (pathCost !? n) ]
        (frontier', pathCost') = L.foldr updateBoth (frontier, pathCost) relevant
        updateBoth (n, cc) (front, pathC) = (update n cc front, updateCost n s cc pathC)


clockwise d
  | d == north = east
  | d == east  = south
  | d == south = west
  | otherwise  = north

counterclockwise d
  | d == north = west
  | d == west  = south
  | d == south = east
  | otherwise  = north

type Point = Int
type State = (Pos, Direction)

neighbours :: Grid -> State -> [(State, Point)]
neighbours grid (from, dir) = [ ((to, dir), 1) | let to = from |+| dir
                                               , U.bounds grid `U.inRange` to
                                               , grid ! to /= '#' ] ++
                              [ ((from, rot dir), 1000) | rot <- [clockwise, counterclockwise] ]


lowestScoring grid start goal = fromMaybe (error "dijkstra couldn't find a path") $
                                dijkstra next found initial
  where next = neighbours grid
        found (p, _) = p == goal
        initial = (start, east)

indexOf :: (U.IArray a e, U.Ix i, Eq e) => a i e -> e -> Maybe i
indexOf arr x = go (U.indices arr)
  where go []     = Nothing
        go (i:is)
          | arr ! i == x = Just i
          | otherwise    = go is

part1 :: Input -> Int
part1 grid = fst $ lowestScoring grid start goal
  where Just start = indexOf grid 'S'
        Just goal = indexOf grid 'E'
answer1 = part1 <$> input

follow next starting = L.foldl' go Set.empty starting
  where go seen x
          | x `Set.member` seen = seen
          | otherwise = L.foldl' go (Set.insert x seen) $ next x

reconstruct :: Pos -> Map.HashMap State (cost, Set State) -> Set Pos
reconstruct goal pathCosts = Set.map fst $ follow next starting
  where next s = snd (pathCosts Map.! s)
        starting = [ s | d <- directions, let s = (goal, d) , s `Map.member` pathCosts]

part2 :: Input -> Int
part2 grid = Set.size $ reconstruct goal pathCosts
  where Just start = indexOf grid 'S'
        Just goal = indexOf grid 'E'
        (_, pathCosts) = lowestScoring grid start goal

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
