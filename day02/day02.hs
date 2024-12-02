{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

test =  map parse [ "7 6 4 2 1"
                  , "1 2 7 8 9"
                  , "9 7 6 2 1"
                  , "1 3 2 4 5"
                  , "8 6 4 4 1"
                  , "1 3 6 7 9"
                  ]
input = map parse . lines <$> readFile "input.txt"

type Level = Int
type Report = [Level]
type Input = [Report]

parse :: String -> Report
parse = map read . words

isSafe xs@(x1:x2:_) = loop xs
  where
    ord = compare x1 x2
    step x y = let diff = abs $ x - y in
               1 <= diff && diff <= 3 && compare x y == ord
    loop (x1:x2:xs) = step x1 x2 && loop (x2:xs)
    loop _ = True
isSafe _ = True


part1 :: Input -> Int
part1 = length . filter isSafe
answer1 = part1 <$> input

isKindaSafe report = any isSafe reports
  where reports = report : [ pre ++ drop 1 post
                           | i <- [0 .. length report - 1],
                             let (pre, post) = L.splitAt i report]

part2 :: Input -> Int
part2 = length . filter isKindaSafe
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
