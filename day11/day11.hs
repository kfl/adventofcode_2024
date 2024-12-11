{-# LANGUAGE ViewPatterns #-}
module Main where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)


test =  parse "125 17"
input = parse <$> readFile "input.txt"

type Input = [Int]

parse :: String -> Input
parse str = map read $ words str

blink n =
  case n of
    0 -> [1]
    (show -> s) | (shifted,  0) <- length s `quotRem` 2,
                  (left, right) <- n `quotRem` (10^shifted) -> [left, right]
    _ -> [n * 2024]

part1 :: Input -> Int
part1 input = length $ blinks !! 25
  where blinks = iterate (>>= blink) input
answer1 = part1 <$> input

type Stones = Map Int Int  -- A multi-map of stones

multiBlink :: Stones -> Stones
multiBlink stones = Map.fromListWith (+) [(stone', c) | (stone, c) <- Map.assocs stones
                                                      , stone' <- blink stone]

part2 :: Input -> Int
part2 input = sum $ blinks !! 75
  where blinks = iterate multiBlink $ Map.fromList $ map (,1) input
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
