{-# LANGUAGE Strict, OverloadedStrings, ViewPatterns #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Short as BS

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import qualified Data.Set as Set
import Data.Set(Set)

import Data.Ord (comparing)

test =  map parse [ "kh-tc"
                  , "qp-kh"
                  , "de-cg"
                  , "ka-co"
                  , "yn-aq"
                  , "qp-ub"
                  , "cg-tb"
                  , "vc-aq"
                  , "tb-ka"
                  , "wh-tc"
                  , "yn-cg"
                  , "kh-ub"
                  , "ta-co"
                  , "de-co"
                  , "tc-td"
                  , "tb-wq"
                  , "wh-td"
                  , "ta-ka"
                  , "td-qp"
                  , "aq-cg"
                  , "wq-ub"
                  , "ub-vc"
                  , "de-ta"
                  , "wq-aq"
                  , "wq-vc"
                  , "wh-yn"
                  , "ka-de"
                  , "kh-ta"
                  , "co-tc"
                  , "wh-qp"
                  , "tb-vc"
                  , "td-yn"
                  ]
input = map parse . B.lines <$> B.readFile "input.txt"


type Node = BS.ShortByteString
type Edge = (Node, Node)
type Graph = Map Node (Set Node)

type Input = [Edge]

parse :: B.ByteString -> Edge
parse str = (BS.toShort $ B.take 2 str, BS.toShort $ B.takeEnd 2 str)

toGraph :: Input -> Graph
toGraph = L.foldl' (\acc (x,y) -> addNeighbour x y $ addNeighbour y x acc) Map.empty
  where addNeighbour x y = Map.insertWith Set.union x $ Set.singleton y

triangles :: Graph -> [(Node, Node, Node)]
triangles g = [ (x,y,z) | (x, ys) <- Map.assocs g
                        , y <- Set.toList ys
                        , z <- Set.toList $ g Map.! y
                        , z `Set.member` ys
                        , any ("t" `BS.isPrefixOf`) [x,y,z] ]

part1 :: Input -> Int
part1 input = length (triangles $ toGraph input) `div` 6
answer1 = part1 <$> input


neighbours :: Graph -> Node -> Set Node
neighbours g v = Map.findWithDefault Set.empty v g

largestClique :: Graph -> Set Node
largestClique g = search Set.empty (Map.keysSet g) Set.empty
  where
    -- Branch-and-bound algorithm to find the maximum clique.
    -- `cur` is the current clique, `candidates` are potential nodes we can still add,
    -- and `bestSoFar` is the best clique we’ve found up to now.
    search
      :: Set Node      -- ^ current clique (R)
      -> Set Node      -- ^ candidate set
      -> Set Node      -- ^ best clique so far
      -> Set Node      -- ^ returns updated best clique
    search cur candidates bestSoFar
      -- 1. Prune if even adding *all* candidates can't beat current best
      | Set.size cur + Set.size candidates <= Set.size bestSoFar = bestSoFar

      -- 2. No more candidates => check if we've improved on bestSoFar
      | Set.null candidates =
        if Set.size cur > Set.size bestSoFar
        then cur
        else bestSoFar

    -- 3. Branch: pick a candidate 'v'
    search cur (Set.deleteFindMin -> (v, rest)) bestSoFar = search cur rest better
      where
        better = search (Set.insert v cur) (Set.intersection rest $ neighbours g v) bestSoFar

part2 :: Input -> BS.ShortByteString
part2 input = BS.intercalate "," $ Set.toList $ largestClique $ toGraph input
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  B.putStrLn $ BS.fromShort $ part2 inp
