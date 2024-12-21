{-# LANGUAGE LambdaCase, Strict #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Data.MemoUgly (memo)

test =  [ "029A"
        , "980A"
        , "179A"
        , "456A"
        , "379A"
        ]
input = lines <$> readFile "input.txt"

type Row = Int
type Col = Int
type Pos = (Col, Row)

type Grid = Map Char Pos
type Input = [String]

(a,b) |-| (c,d) = (a-c, b-d)

toPos rows = Map.fromList letters
  where letters = [ (c, (x,y)) | (y, row) <- zip [0..] rows
                               , (x, c) <- zip [0..] row]

keypad, dirpad :: Grid
keypad = toPos ["789", "456", "123", " 0A"]
dirpad = toPos [" ^A", "<v>"]

encode (x, y, vert_first) = cc ++ "A"
  where rep = replicate
        xx = rep (-x) '<' ++ rep x '>'
        yy = rep y 'v' ++ rep (-y) '^'
        cc = if vert_first then yy++xx else xx++yy

path = memo p
  where
    p (from, to) = encode (dx, dy, vert_first)
      where pad = if Map.member from keypad && Map.member to keypad then keypad else dirpad
            start = pad Map.! from
            end = pad Map.! to
            (dx, dy) = end |-| start
            bad = pad Map.! ' ' |-| start
            vert_first = (dx > 0 || bad == (dx, 0)) && bad /= (0, dy)

steps = memo s
  where s (code, 0) = length code
        s (code, depth) = snd $ L.foldl' step ('A', 0) code
          where
            step (from, acc) to = (to, steps(path (from, to), depth-1) + acc)

complexity :: Int -> String -> Int
complexity depth code = steps (code, depth) * (read $ take 3 code)

part1 :: Input -> Int
part1 input = sum $ map (complexity 3) input
answer1 = part1 <$> input

part2 :: Input -> Int
part2 input = sum $ map (complexity 26) input
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
