module Main where

import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Either as E

test =  parse $ unlines [ "#####"
                        , ".####"
                        , ".####"
                        , ".####"
                        , ".#.#."
                        , ".#..."
                        , "....."
                        , ""
                        , "#####"
                        , "##.##"
                        , ".#.##"
                        , "...##"
                        , "...#."
                        , "...#."
                        , "....."
                        , ""
                        , "....."
                        , "#...."
                        , "#...."
                        , "#...#"
                        , "#.#.#"
                        , "#.###"
                        , "#####"
                        , ""
                        , "....."
                        , "....."
                        , "#.#.."
                        , "###.."
                        , "###.#"
                        , "###.#"
                        , "#####"
                        , ""
                        , "....."
                        , "....."
                        , "....."
                        , "#...."
                        , "#.#.."
                        , "#.#.#"
                        , "#####"
                        ]
input = parse <$> readFile "input.txt"

type LockKey = Either [Int] [Int]
type Input = [LockKey]

parse str = map lockkey lockkeys
  where lockkeys = map lines $ L.splitOn "\n\n" str
        lockkey rows@((c:_):_) = con [length (filter (== '#') col) - 1 | col <- L.transpose rows]
          where con = case c of '#' -> Left; _ -> Right

overlap xs ys = any (> 5) $ zipWith (+) xs ys

part1 :: Input -> Int
part1 input = length [ (k, l) | k <- keys, l <- locks, not $ overlap k l]
  where (locks, keys) = E.partitionEithers input
answer1 = part1 <$> input

part2 :: Input -> String
part2 _ = "Merry XMAS"
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  putStrLn $ part2 inp
