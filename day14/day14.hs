{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Text.Regex.TDFA as RE
import Text.Regex.TDFA ((=~))

import Data.Bifunctor (bimap)
import Control.Monad (forM_)


test :: Input
test = ((11,7),
        map parse [ "p=0,4 v=3,-3"
                  , "p=6,3 v=-1,-3"
                  , "p=10,3 v=-1,2"
                  , "p=2,0 v=2,-1"
                  , "p=0,0 v=1,3"
                  , "p=3,0 v=-2,-2"
                  , "p=7,6 v=-1,-3"
                  , "p=3,0 v=-1,-2"
                  , "p=9,3 v=2,3"
                  , "p=7,3 v=-1,2"
                  , "p=2,4 v=2,-3"
                  , "p=9,5 v=-3,-3"
                  ])
input = ((101, 103), ) . map parse . lines <$> readFile "input.txt"

type Row = Int
type Col = Int
type Pos = (Col, Row)
type Vel = (Int, Int)
data Robot = R !Pos !Vel
  deriving (Eq, Show)

type Input = ((Int, Int), [Robot])

parse :: String -> Robot
parse str = R (px,py) (vx, vy)
  where ds = RE.getAllTextMatches $ str =~ "(-?[[:digit:]]+)"
        [px, py, vx, vy] = map read ds


(a, b) |+| (c, d) = (a+c, b+d)
wrap maxX maxY = bimap (`mod` maxX) (`mod` maxY)

move w (R pos vel) = R (w $ pos |+| vel) vel

data Quadrant = Q {q1, q2, q3, q4 :: Int}
  deriving (Eq, Show)

instance Semigroup Quadrant where
  (Q x y z w) <> (Q a b c d) = Q (x+a) (y+b) (z+c) (w+d)

instance Monoid Quadrant where
  mempty = Q 0 0 0 0

count halfC halfR (R (x,y) _)
  | x == halfC = mempty
  | y == halfR = mempty
  | otherwise = case (x `div` halfC, y `div` halfR) of
                  (0, 0) -> Q 1 0 0 0
                  (0, _) -> Q 0 1 0 0
                  (_, 0) -> Q 0 0 1 0
                  _      -> Q 0 0 0 1

part1 :: Input -> Int
part1 ((maxC, maxR), robots) = q1 * q2 * q3 * q4
  where moves = iterate (map (move $ wrap maxC maxR)) robots
        (halfC, halfR) = (maxC `div` 2, maxR `div` 2)
        Q q1 q2 q3 q4 = foldMap (count halfC halfR) $ moves !! 100
answer1 = part1 <$> input


potential1 halfC robots = 20 < length [ r | r@(R (x,_) _) <- robots, x == halfC ]

longestStaight :: [Int] -> Int
longestStaight ys = longest
  where
    step (prev, currLen, maxLen) y
      | y == prev + 1 = (y, currLen + 1, max maxLen (currLen + 1))
      | otherwise     = (y, 1, max maxLen currLen)
    (_, _, longest) = L.foldl' step (-2, 0, 0) $ L.sort ys

potential robots =
  any ((10 <) . longestStaight) $ Map.elems $ Map.fromListWith (++) [ (x, [y]) | r@(R (x,y) _) <- robots ]

marks robots = Set.fromList $ map (\(R p _) -> p) robots

showGrid maxC maxR markings = do
  forM_ [0 .. maxR] $ \y -> do
    forM_ [0 .. maxC] $ \x -> do
       putStr $ if (x,y) `Set.member` markings then "*" else " "
    putStrLn ""


part2' :: Input -> IO ()
part2' ((maxC, maxR), robots) = do
  forM_ (take 3 potentials) $ \(i, rs) -> do
    showGrid maxC maxR $ marks rs
    putStrLn $ "step " ++ show i
  where moves = zip [0..] (iterate (map (move $ wrap maxC maxR)) robots)
        (halfC, halfR) = (maxC `div` 2, maxR `div` 2)
        potentials = filter (potential . snd) moves

part2 :: Input -> Int
part2 ((maxC, maxR), robots) = i
  where moves = iterate (map (move $ wrap maxC maxR)) robots
        Just i = L.findIndex potential moves

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
