{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L

import Text.Regex.TDFA ((=~))

import Data.MemoUgly (memo)
import Data.Maybe (mapMaybe)

import qualified Data.ByteString.Char8 as C

test =  parse [ "r, wr, b, g, bwu, rb, gb, br"
              , ""
              , "brwrr"
              , "bggr"
              , "gbbr"
              , "rrbgbr"
              , "ubwu"
              , "bwurrg"
              , "brgr"
              , "bbrgwb"
              ]
input = parse . C.lines <$> C.readFile "input.txt"

type Input = ([C.ByteString], [C.ByteString])

parse (pats : _ : designs) = (map C.dropSpace $ C.split ',' pats, designs)

part1 :: Input -> Int
part1 (pats, designs) = length $ filter (=~ desired) designs
  where alphabet = C.intercalate "|" pats
        desired = C.concat ["^(", alphabet, ")*$"]
answer1 = part1 <$> input

part1' :: Input -> Int
part1' (pats, designs) = length $ filter possible designs
  where possible = memo $ \case "" -> True
                                s  -> any possible $ mapMaybe (`C.stripPrefix` s) pats


part2 :: Input -> Int
part2 (pats, designs) = sum $ map ways designs
  where ways = memo $ \case "" -> 1
                            s  -> sum $ map ways $ mapMaybe (`C.stripPrefix` s) pats

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1' inp
  print $ part2 inp
