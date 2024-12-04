module Main where

import qualified Data.Array.Unboxed as U
import Data.Array.Unboxed ((!))
import Control.Monad (forM_)



test = parse [ "MMMSXXMASM"
             , "MSAMXMSMSA"
             , "AMXSXMAAMM"
             , "MSAMASMSMX"
             , "XMASAMXAMM"
             , "XXAMMXXAMA"
             , "SMSMSASXSS"
             , "SAXAMASAAA"
             , "MAMMMXMMMM"
             , "MXMXAXMASX"
             ]
input = parse . lines <$> readFile "input.txt"

type Grid = U.Array (Int, Int) Char
type Input = Grid

parse :: [String] -> Grid

parse rows@(cols : _) = U.array bounds letters
  where letters = [ ((x,y), c) | (y, row) <- zip [0..] rows
                               , (x, c) <- zip [0..] row]
        bounds = ((0,0), (length cols - 1, length rows - 1))

showGrid show grid = do
  forM_ [miny .. maxy] $ \y -> do
    forM_ [minx .. maxx] $ \x -> do
       putStr $ show $ grid ! (x,y)
    putStrLn ""
  where ((minx, miny), (maxx, maxy)) = U.bounds grid

(a, b) |+| (c, d) = (a+c, b+d)


xmasSearch grid idx@(x,y) =
  [ word | line <- [U.range (idx, idx |+| (0, 3)),
                    U.range (idx, idx |+| (3, 0)),
                    [ idx |+| (i,i) | i <- [0..3]],
                    [ idx |+| (-i,i) | i <- [0..3]]]
         , all (U.bounds grid `U.inRange`) line
         , let word = map (grid !) line
         , word == "XMAS" || word == "SAMX"]

searchGrid :: Grid -> (Grid -> (Int, Int) -> [a]) -> [a]
searchGrid grid searchAt = concat [ searchAt grid (x,y) | x <- [minx .. maxx]
                                                        , y <- [miny .. maxy] ]
  where ((minx, miny), (maxx, maxy)) = U.bounds grid


part1 :: Input -> Int
part1 input = length $ searchGrid input xmasSearch
answer1 = part1 <$> input

xmasSearch2 grid idx =
  [ idx | let (line1, line2) = ([ idx |+| (i, i) | i <- [-1..1]],
                                [ idx |+| (i, - i) | i <- [-1..1]])
         , all (U.bounds grid `U.inRange`) line1
         , all (U.bounds grid `U.inRange`) line2
         , let word1 = map (grid !) line1
         , let word2 = map (grid !) line2
         , word1 == "MAS" || word1 == "SAM"
         , word2 == "MAS" || word2 == "SAM"]

part2 :: Input -> Int
part2 input = length $ searchGrid input xmasSearch2
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
