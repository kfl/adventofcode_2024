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
import qualified Control.Monad.Par as Par
import qualified Data.Vector.Unboxed as V



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
  where to5Bits !x = x + 9 .&. 0x1F  -- 0x1F = 31 in decimal

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


part2' :: Input -> Int
part2' secrets = maximum allWindowPrices
  where
    priceChange :: Int -> V.Vector (Int, Int)
    priceChange secret = V.zip prices (V.cons 0 $ V.zipWith (-) (V.tail prices) prices)
      where prices = V.map (`mod` 10) $ V.iterateN 2000 nextNum secret

    windowPrice :: V.Vector (Int, Int) -> [(Int, Int)]
    windowPrice pc = L.unfoldr step 0
      where
        len = V.length pc
        step i
          | i + 3 < len =
            let (_, a) = pc V.! i
                (_, b) = pc V.! (i + 1)
                (_, c) = pc V.! (i + 2)
                (p, d) = pc V.! (i + 3)
            in  Just ((pack4 a b c d, p), i + 1)
          | otherwise = Nothing

    windowPrices secret = IntMap.fromListWith (\_ y -> y)
                          $ windowPrice
                          $ priceChange secret

    secretsV = V.fromList secrets
    mapper i = return $ windowPrices (secretsV V.! i)
    range = Par.InclusiveRange 0 (V.length secretsV - 1)
    combine x y = return $ IntMap.unionWith (+) x y

    allWindowPrices = runPar $ Par.parMapReduceRange range mapper combine IntMap.empty

answer2 = part2' <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2' inp
