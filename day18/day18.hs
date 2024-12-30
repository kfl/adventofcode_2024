{-# LANGUAGE ViewPatterns #-}
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
import qualified Data.HashMap.Strict as HMap

import Data.Foldable (asum)


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
         -> Maybe (cost, [state])
dijkstra next found initial = loop initPathCost initPrev startFrontier
  where
    x `less` may = maybe True (x <) may
    m !? state = HMap.lookup state m

    update n c = Q.insert n c ()

    startFrontier = Q.singleton initial 0 ()
    initPathCost  = HMap.singleton initial 0
    initPrev      = HMap.empty

    loop _ _ (Q.minView -> Nothing) = Nothing
    loop pathCost prev (Q.minView -> Just(s, c, _, frontier))
      | found s = Just (c, reconstruct s prev)
      | otherwise = loop pathCost' prev' frontier'
      where
        relevant = [ (n, cc) | (n, sc) <- next s,
                               let cc = c + sc,
                               cc `less` (pathCost !? n) ]
        (frontier', pathCost', prev') = L.foldr updateAll (frontier, pathCost, prev) relevant
        updateAll (n, cc) (front, pathC, prev) =
              (update n cc front, HMap.insert n cc pathC, HMap.insert n s prev)

    reconstruct goal prev = reverse $ go [] goal
      where go acc cur = maybe (cur:acc) (go (cur:acc)) $ HMap.lookup cur prev

part1 :: Int -> Int -> Int -> Input -> Int
part1 maxC maxR drops positions = maybe (error "dijkstra couldn't find a path") fst
                                  $ dijkstra next found start
  where corrupted = Set.fromList $ take drops positions
        start = (0,0)
        goal = (maxC, maxR)
        next p = [ (p', 1) | d <- directions, let p' = p |+| d
                                            , (start, goal) `I.inRange` p'
                                            , p' `Set.notMember` corrupted ]
        found = (== goal)

answer1 = part1 70 70 1024 <$> input

part2 :: Int -> Int -> Input -> Pos
part2 maxC maxR positions = maybe (error "There's always an exit")
                                  L.last blocked
  where start = (0,0)
        goal = (maxC, maxR)
        next corrupted p = [ (p', 1) | d <- directions
                                     , let p' = p |+| d
                                     , (start, goal) `I.inRange` p'
                                     , p' `Set.notMember` corrupted ]
        found = (== goal)
        blocked = L.find (\ps -> isNothing $ dijkstra (next $ Set.fromList ps) found start) $ L.inits positions


firstRight = asum . map (either (const Nothing) Just)

part2' :: Int -> Int -> Input -> Pos
part2' maxC maxR positions = fromMaybe (error "There's always an exit") bad
  where start = (0,0)
        goal = (maxC, maxR)
        next corrupted p = [ (p', 1) | d <- directions
                                     , let p' = p |+| d
                                     , (start, goal) `I.inRange` p'
                                     , p' `Set.notMember` corrupted ]
        found = (== goal)

        check acc@(Right _) _ = acc
        check (Left (cor, path)) p
          | p `Set.notMember` path = Left(Set.insert p cor, path)
          | cor' <- Set.insert p cor
          , Just (_, new) <- dijkstra (next cor') found start = Left (cor', Set.fromList new)
          | otherwise = Right p

        firstPath = ((0,) <$> [0..70]) ++ ((,70) <$> [1..70])

        blocked = L.scanl' check (Left (Set.empty, Set.fromList firstPath)) positions
        bad = firstRight blocked


answer2 = part2' 70 70 <$> input

main = do
  print =<< answer1
  print =<< answer2
