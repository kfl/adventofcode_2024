{-# LANGUAGE LambdaCase, Strict #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Text.Regex.TDFA ((=~))
import qualified Data.MemoTrie as Memo
import Data.Maybe (mapMaybe)


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
input = parse . lines <$> readFile "input.txt"

type Input = ([String], [String])

parse (pats : _ : designs) = (L.wordsBy (`elem` ", ") pats, designs)

part1 :: Input -> Int
part1 (pats, designs) = length $ filter (=~ desired) designs
  where alphabet = L.intercalate "|" pats
        desired = "^("++alphabet++")*$"
answer1 = part1 <$> input


part2 :: Input -> Int
part2 (pats, designs) = sum $ map ways designs
  where ways = Memo.memo $ \case "" -> 1
                                 s  -> sum $ map ways $ mapMaybe (`L.stripPrefix` s) pats

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
