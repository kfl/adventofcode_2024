{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Bifunctor (bimap)


test =  parse $ unlines [ "47|53"
                        , "97|13"
                        , "97|61"
                        , "97|47"
                        , "75|29"
                        , "61|13"
                        , "75|53"
                        , "29|13"
                        , "97|29"
                        , "53|29"
                        , "61|53"
                        , "97|53"
                        , "61|29"
                        , "47|13"
                        , "75|47"
                        , "97|75"
                        , "47|61"
                        , "75|61"
                        , "47|29"
                        , "75|13"
                        , "53|13"
                        , ""
                        , "75,47,61,53,29"
                        , "97,61,53,29,13"
                        , "75,29,13"
                        , "75,97,47,61,53"
                        , "61,13,29"
                        , "97,13,75,29,47"
                        ]
input = parse <$> readFile "input.txt"

type Page = Int
type Rule = (Page, Page)
type Update = [Page]
type Input = ([Rule], [Update])

parse :: String -> Input
parse str = (rules, updates)
  where [ruless, updatess] = L.splitOn "\n\n" str
        rules = map rule $ lines ruless
        updates = map update $ lines updatess
        rule s = bimap read (read . drop 1) $ break (== '|') s
        update s = read $ "[" <> s <> "]"

rulesMap rules = L.foldl' upsert Map.empty rules
  where upsert acc (x, y) =
          Map.alter (\case Nothing -> Just $ Set.singleton y
                           Just afters -> Just $ Set.insert y afters) x acc

middle xs = xs L.!! (length xs `div` 2)

correct _ [] = True
correct rm (p:ps) = all (`Set.member` Map.findWithDefault Set.empty p rm) ps
                    && correct rm ps

part1 :: Input -> Int
part1 (rules, updates) = sum middles
  where rm = rulesMap rules
        corrects = filter (correct rm) updates
        middles = map middle corrects

answer1 = part1 <$> input

part2 :: Input -> Int
part2 (rules, updates) = sum middles
  where rm = rulesMap rules
        incorrects = filter (not . correct rm) updates
        cmp x y
          | x `Set.member` Map.findWithDefault Set.empty y rm = GT
          | y `Set.member` Map.findWithDefault Set.empty x rm = LT
        corrected = map (L.sortBy cmp) incorrects
        middles = map middle corrected

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
