{-# LANGUAGE LambdaCase, Strict, ViewPatterns #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Ix as I
import Data.Maybe (fromMaybe, isNothing)


import qualified Data.Hashable as H
import qualified Data.HashPSQ as Q


test =  map parse [ "5,4"
                  , "4,2"
                  , "4,5"
                  , "3,0"
                  , "2,1"
                  , "6,3"
                  , "2,4"
                  , "1,5"
                  , "0,6"
                  , "3,3"
                  , "2,6"
                  , "5,1"
                  , "1,2"
                  , "5,5"
                  , "2,5"
                  , "6,5"
                  , "1,4"
                  , "0,4"
                  , "6,4"
                  , "1,1"
                  , "6,1"
                  , "1,0"
                  , "0,5"
                  , "1,6"
                  , "2,0"
                  ]
input = map parse . lines <$> readFile "input.txt"

type Row = Int
type Col = Int
type Pos = (Col, Row)

type Input = [Pos]

type Direction = (Int, Int)
directions :: [Direction]
directions@[north, south, east, west] = [ (0,-1), (0,1), (1,0), (-1,0) ]

(a, b) |+| (c, d) = (a+c, b+d)

parse :: String -> Pos
parse str = read $ "("++str++")"


dijkstra :: (Num cost, Ord cost, Ord state, H.Hashable state)
         => (state -> [(state, cost)])
         -> (state -> Bool)
         -> state
         -> Maybe cost
dijkstra next found initial = loop initPathCost startFrontier
  where
    x `less` may = maybe True (x <) may
    m !? state = Map.lookup state m

    update n c queue = snd $ Q.alter upsert n queue
      where
        upsert Nothing = ((), Just(c, ()))
        upsert (Just(c', _)) = ((), Just(min c c', ()))

    startFrontier = Q.singleton initial 0 ()
    initPathCost = Map.empty

    loop pathCost (Q.minView -> Nothing) = Nothing
    loop pathCost (Q.minView -> Just(s, c, _, frontier))
      | found s = Just c
      | otherwise = loop pathCost' frontier'
      where
        relevant = [ (n, cc) | (n, sc) <- next s,
                               let cc = c + sc,
                               cc `less` (pathCost !? n) ]
        (frontier', pathCost') = L.foldr updateBoth (frontier, pathCost) relevant
        updateBoth (n, cc) (front, pathC) = (update n cc front, Map.insert n cc pathC)

part1 :: Int -> Int -> Int -> Input -> Int
part1 maxC maxR drops positions = fromMaybe (error "dijkstra couldn't find a path") $
                                  dijkstra next found start
  where corrupted = Set.fromList $ take drops positions
        start = (0,0)
        goal = (maxC, maxR)
        next p = [ (p', 1) | d <- directions, let p' = p |+| d
                                            , (start, goal) `I.inRange` p'
                                            , p' `Set.notMember` corrupted ]
        found = (== goal)

answer1 = part1 70 70 1024 <$> input

part2 :: Int -> Int -> Input -> Pos
part2 maxC maxR positions = fromMaybe (error "There's always an exit") $
                            L.last <$> blocked
  where start = (0,0)
        goal = (maxC, maxR)
        next corrupted p = [ (p', 1) | d <- directions
                                     , let p' = p |+| d
                                     , (start, goal) `I.inRange` p'
                                     , p' `Set.notMember` corrupted ]
        found = (== goal)
        blocked = L.find (\ps -> isNothing $ dijkstra (next $ Set.fromList ps) found start) $ L.inits positions

answer2 = part2 70 70 <$> input

main = do
  print =<< answer1
  print =<< answer2
