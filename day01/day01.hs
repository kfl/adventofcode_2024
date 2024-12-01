module Main where

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

test =  map parse [ "3   4"
                  , "4   3"
                  , "2   5"
                  , "1   3"
                  , "3   9"
                  , "3   3"
                  ]
input = map parse . lines <$> readFile "input.txt"

type Input = [(Int, Int)]

parse :: String -> (Int, Int)
parse str = (x, y)
  where [x, y] = map read $ words str

part1 :: Input -> Int
part1 input = sum dists
  where (xs, ys) = unzip input
        dists = zipWith (\x y -> abs $ x - y) (L.sort xs) (L.sort ys)
answer1 = part1 <$> input

part2 :: Input -> Int
part2 input = sum [ x * c | x <- xs, let c = Map.findWithDefault 0 x counts ]
  where (xs, ys) = unzip input
        counts = L.foldl' (flip $ Map.alter (Just . maybe 1 (+1))) Map.empty ys
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
