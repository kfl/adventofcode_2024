module Main where

import qualified Data.List as L
import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Array.Unboxed as U
import Data.Array.Unboxed ((!))
import Control.Monad (forM_)


test =  parse [ "RRRRIICCFF"
              , "RRRRIICCCF"
              , "VVRRRCCFFF"
              , "VVRCCCJFFF"
              , "VVVVCJJCFE"
              , "VVIVCCJJEE"
              , "VVIIICJJEE"
              , "MIIIIIJJEE"
              , "MIIISIJEEE"
              , "MMMISSJEEE"
              ]

input = parse . lines <$> readFile "input.txt"

type Row = Int
type Col = Int
type Plot = (Col, Row)

type Grid = U.Array Plot Char
type Input = Grid

parse :: [String] -> Grid
parse rows@(cols : _) = U.array bounds letters
  where letters = [ ((x,y), c) | (y, row) <- zip [0..] rows
                               , (x, c) <- zip [0..] row]
        bounds = ((0,0), (length cols - 1, length rows - 1))

directions@[north, south, east, west] = [ (0,-1), (0,1), (1,0), (-1,0) ]
(a, b) |+| (c, d) = (a+c, b+d)
neighbours plot = [ plot |+| dir | dir <- directions ]

flood :: Grid -> Set Plot -> (Plot, Char) -> Set Plot
flood plots flooded (plot, c) = L.foldl' (flood plots) (Set.insert plot flooded) possible
  where possible = [ (n, c) | n <- neighbours plot
                            , U.bounds plots `U.inRange` n
                            , c == plots ! n
                            , n `Set.notMember` flooded ]


regions plots = L.foldl' addRegion [] $ U.assocs plots
  where addRegion acc x@(plot, _)
          | any (Set.member plot) acc = acc
          | otherwise = flood plots Set.empty x : acc

perimeter region = sum $ map outer $ Set.toList region
  where outer p = length [ n | n <- neighbours p, Set.notMember n region ]

part1 :: Input -> Int
part1 input = sum $ [ Set.size r * perimeter r | r <- regions input]

answer1 = part1 <$> input


sides :: Set Plot -> Int
sides region = sum $ map corners $ Set.toList region
  where
    different = (`Set.notMember` region)
    same = (`Set.member` region)
    corner dir1 dir2 x =
      fromEnum $ different (x |+| dir1) && (different (x |+| dir2) || same (x |+| dir1 |+| dir2))
    corners x =  sum $ [corner north east, corner west north, corner south west, corner east south] <*> pure x


part2 :: Input -> Int
part2 input = sum $ [ Set.size r * sides r | r <- regions input]
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
