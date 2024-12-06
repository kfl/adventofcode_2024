{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.List as L
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (mapMaybe, fromJust)

import Control.Monad.Par (runPar, parMap)

test =  parse [ "....#....."
              , ".........#"
              , ".........."
              , "..#......."
              , ".......#.."
              , ".........."
              , ".#..^....."
              , "........#."
              , "#........."
              , "......#..."
              ]
input = parse . lines <$> readFile "input.txt"

type Row = Int
type Col = Int
type Input = (Int, Int, [((Col, Row), Bool)])


parse :: [String] -> Input
parse rows@(cols: _) = (length cols, length rows,
                        [ ((x,y), c == '#') | (y, row) <- zip [0..] rows
                                            , (x, c) <- zip [0..] row
                                            , c `elem` "^#"])

data Direction = North | East | South | West
  deriving (Eq, Show, Ord, Enum)


floormap obstacles =
  Set.fromList $ mapMaybe (\case (p, True) -> Just p; _ -> Nothing) obstacles

next (dir, (c,r)) =
  case dir of
    North -> (c, r-1)
    South -> (c, r+1)
    East  -> (c+1, r)
    West  -> (c-1, r)

turnRight (dir, p) = (if dir == West then North else succ dir, p)

coveredArea (maxCol, maxRow, obstacles) = snd $ last steps
  where floor = floormap obstacles
        start = fst . fromJust $ L.find (not.snd) obstacles
        step (curr@(dir, p), seen) = if n `Set.member` floor then (turnRight curr, seen)
                                     else ((dir, n), Set.insert n seen)
          where n = next curr
        inBounds (_, (c,r)) = 0 <= c && c < maxCol && 0 <= r && r < maxRow
        steps = takeWhile (inBounds . fst) $ iterate step ((North, start), Set.singleton start)

part1 :: Input -> Int
part1 input = Set.size $ coveredArea input

answer1 = part1 <$> input

part2 :: Input -> Int
part2 input@(maxCol, maxRow, obstacles) = length loopings
  where floor = floormap obstacles
        start = (North, fst . fromJust $ L.find (not.snd) obstacles)
        covered = coveredArea input

        step floor (curr@(dir, p), seen) = (new, Set.insert new seen)
          where n = next curr
                new = if n `Set.member` floor then turnRight curr
                      else (dir, n)

        inBounds (_, (c,r)) = 0 <= c && c < maxCol && 0 <= r && r < maxRow

        isLooping (x:y:rest)
          | not $ inBounds $ fst x = False
          | snd x == snd y = True
          | otherwise = isLooping $ y : rest

        loops p = isLooping $ iterate (step $ Set.insert p floor) (start, Set.singleton start)
        loopings =
          filter id $ runPar $ parMap loops $ Set.toList $ Set.delete (snd start) covered

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
