{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Data.Maybe (fromMaybe)
import Data.Bifunctor (second)

import Data.Sequence (Seq (..), (><), (<|), (|>), ViewR(..))
import qualified Data.Sequence as Seq


test =  parse "2333133121414131402"
input = parse <$> readFile "input.txt"

type Input = [Int]

parse :: String -> Input
parse = map C.digitToInt . filter C.isDigit

type Id = Int
type Size = Int
data OnDisk = File !Id !Size
            | Free !Size
            deriving (Eq, Show, Ord)
type Layout = Seq OnDisk

toLayout :: Input -> Layout
toLayout diskmap = expanded
  where expand (layout, next, free) sz
          | free = (layout |> Free sz, next, not free)
          | otherwise = (layout |> File next sz, next+1, not free)
        (expanded, _, _) = L.foldl' expand (Seq.empty, 0, False) diskmap

defrag :: Layout -> Layout
defrag layout = go Empty layout
  where go done = \case
          Empty                                        -> done
          Free 0 :<| rest                              -> go done rest
          rest :|> Free _                              -> go done rest
          file@(File _ _) :<| rest                     -> go (done |> file) rest
          (Free m :<| middle) :|> (File id n) | m >= n -> go (done |> File id n) (Free (m - n) <| middle)
          (Free m :<| middle) :|> (File id n)          -> go (done |> File id m) (middle |> File id (n - m))

checksum :: Layout -> Int
checksum layout = fst $ L.foldl' csum (0, 0) layout
  where csum (acc, i) = \case
          Free n -> (acc, i+n)
          File id n -> (acc + sum [j*id | j <- [i..i+n-1]], i+n)


part1 :: Input -> Int
part1 input = checksum $ defrag $ toLayout input
answer1 = part1 <$> input

showLayout layout = concatMap (\case Free n -> replicate n '.'; File id n -> concat $ replicate n $ show id) layout

step = \case
  Empty -> Nothing
  rest :|> Free n -> (|> Free n) <$> step rest
  Free 0 :<| rest -> pure rest
  rest :|> file@(File _ n) -> case move file rest of
                                Just modified -> pure $ modified |> Free n
                                Nothing -> (|> file) <$> step rest

move file@(File _ n) = \case
  Empty                     -> Nothing
  Free m :<| rest  | m >= n -> pure $ file <| Free (m - n) <| rest
  f :<| rest                -> (f <|) <$> move file rest

defrag2 :: Layout -> Layout
defrag2 layout = res
  where
    _ :|> res = Seq.unfoldr (\s -> (\x->(x,x)) <$> step s) layout

part2 :: Input -> Int
part2 input = checksum $ defrag2 $ toLayout input
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
