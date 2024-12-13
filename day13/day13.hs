module Main where

import qualified Data.List.Split as L

import qualified Text.Regex.TDFA as RE
import Text.Regex.TDFA ((=~))

import Data.Maybe(mapMaybe)

test =  parse $ unlines [ "Button A: X+94, Y+34"
                        , "Button B: X+22, Y+67"
                        , "Prize: X=8400, Y=5400"
                        , ""
                        , "Button A: X+26, Y+66"
                        , "Button B: X+67, Y+21"
                        , "Prize: X=12748, Y=12176"
                        , ""
                        , "Button A: X+17, Y+86"
                        , "Button B: X+84, Y+37"
                        , "Prize: X=7870, Y=6450"
                        , ""
                        , "Button A: X+69, Y+23"
                        , "Button B: X+27, Y+71"
                        , "Prize: X=18641, Y=10279"
                        ]
input = parse <$> readFile "input.txt"


data Machine = M !Int !Int !Int !Int !Int !Int
  deriving (Eq, Show)
type Input = [Machine]

parse :: String -> Input
parse str = map machine machines
  where machines = map lines $ L.splitOn "\n\n" str
        machine m = M ax ay bx by px py
          where ns = concatMap (\s -> RE.getAllTextMatches $ s =~ "([[:digit:]]+)") m
                [ax, ay, bx, by, px, py] = map read ns


solution :: Machine -> Maybe Int
solution (M ax ay bx by px py) =
  case [ 3*i + j | i <- [0..100], j <- [0..100]
                 , i*ax + j*bx == px
                 , i*ay + j*by == py ] of
    [] -> Nothing
    solutions -> Just $ minimum solutions

part1 :: Input -> Int
part1 = sum . mapMaybe solution
answer1 = part1 <$> input

adjust (M ax ay bx by px py) = M ax ay bx by (10000000000000 + px) (10000000000000 + py)

solve (M ax ay bx by px py) =
  case d of
    0                            -> Nothing -- No solution (or infinitely many)
    _ | (i, 0) <- di `quotRem` d
      , (j, 0) <- dj `quotRem` d -> Just $ 3*i + j
    _                            -> Nothing -- No integer solution
  where d  = ax*by - ay*bx
        di = by*px - bx*py
        dj = ax*py - ay*px

part2 :: Input -> Int
part2 = sum . mapMaybe (solve . adjust)
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
