{-# LANGUAGE Strict #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as L

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.Int (Int64, Int8)
import Data.Bits (xor, Bits, shiftL, shiftR, (.&.), (.|.))
import Control.Monad.Par (runPar, parMap)


test =  map parse [ "1"
                  , "10"
                  , "100"
                  , "2024"
                  ]
input = map parse . lines <$> readFile "input.txt"

type Input = [Int]

parse :: String -> Int
parse = read

prune :: Int -> Int
prune x = x `mod` 16777216

mix :: Int -> Int -> Int
mix = xor

nextNum :: Int -> Int
nextNum !secret = step3
  where -- Step 1: multiply by 64 (shift left by 6)
        step1 = prune $ secret `mix` (secret `shiftL` 6)
        -- Step 2: divide by 32 (shift right by 5)
        step2 = prune $ step1 `mix` (step1 `shiftR` 5)
        -- Step 3: multiply by 2048 (shift left by 11)
        !step3 = prune $ step2 `mix` (step2 `shiftL` 11)

part1 :: Input -> Int
part1 input = sum $ map iter2000 input
  where iter2000 x = iterate nextNum x !! 2000
answer1 = part1 <$> input

pack4 :: Int -> Int -> Int -> Int -> Int
pack4 a b c d = to5Bits a
                .|. (to5Bits b `shiftL` 5)
                .|. (to5Bits c `shiftL` 10)
                .|. (to5Bits d `shiftL` 15)
  where to5Bits x = x + 9 .&. 0x1F  -- 0x1F = 31 in decimal

part2 :: Input -> Int
part2 secrets = maximum $ allWindowPrices secrets
  where
    priceChange secret = zip prices $ 0 : zipWith (-) (drop 1 prices) prices
      where prices = map (`mod` 10) $ iterate nextNum secret

    windowPrices secret = IntMap.fromListWith (\_ y -> y)
                          $ map windowPrice
                          $ L.divvy 4 1
                          $ take 2000
                          $ priceChange secret
      where windowPrice [(_,a), (_,b), (_,c), (!p,d)] = (pack4 a b c d, p)

    allWindowPrices = IntMap.unionsWith (+) . runPar . parMap windowPrices

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
