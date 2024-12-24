{-# LANGUAGE LambdaCase, Strict #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Data.Bifunctor (second)

test =  parse $ unlines [ "x00: 1"
                        , "x01: 0"
                        , "x02: 1"
                        , "x03: 1"
                        , "x04: 0"
                        , "y00: 1"
                        , "y01: 1"
                        , "y02: 1"
                        , "y03: 1"
                        , "y04: 1"
                        , ""
                        , "ntg XOR fgs -> mjb"
                        , "y02 OR x01 -> tnw"
                        , "kwq OR kpj -> z05"
                        , "x00 OR x03 -> fst"
                        , "tgd XOR rvg -> z01"
                        , "vdt OR tnw -> bfw"
                        , "bfw AND frj -> z10"
                        , "ffh OR nrd -> bqk"
                        , "y00 AND y03 -> djm"
                        , "y03 OR y00 -> psh"
                        , "bqk OR frj -> z08"
                        , "tnw OR fst -> frj"
                        , "gnj AND tgd -> z11"
                        , "bfw XOR mjb -> z00"
                        , "x03 OR x00 -> vdt"
                        , "gnj AND wpb -> z02"
                        , "x04 AND y00 -> kjc"
                        , "djm OR pbm -> qhw"
                        , "nrd AND vdt -> hwm"
                        , "kjc AND fst -> rvg"
                        , "y04 OR y02 -> fgs"
                        , "y01 AND x02 -> pbm"
                        , "ntg OR kjc -> kwq"
                        , "psh XOR fgs -> tgd"
                        , "qhw XOR tgd -> z09"
                        , "pbm OR djm -> kpj"
                        , "x03 XOR y03 -> ffh"
                        , "x00 XOR y04 -> ntg"
                        , "bfw OR bqk -> z06"
                        , "nrd XOR fgs -> wpb"
                        , "frj XOR qhw -> z04"
                        , "bqk OR frj -> z07"
                        , "y03 OR x01 -> nrd"
                        , "hwm AND bqk -> z03"
                        , "tgd XOR rvg -> z12"
                        , "tnw OR pbm -> gnj"
                        ]
input = parse <$> readFile "input.txt"

type Wire = String
data Opr = AND | OR | XOR
  deriving (Eq, Show, Read, Ord)
type Expr = (Wire, Opr, Wire)
type Circuit = Map Wire Expr

type Input = ([(Wire, Bool)], Circuit)

parse :: String -> Input
parse str = (map initialisation inits, Map.fromList $ map wirering wires)
  where [inits, wires] = map lines $ L.splitOn "\n\n" str
        initialisation s = second (toEnum . read . drop 1) $ L.break (== ':') s
        wirering s = (dst, (lhs, read opr, rhs))
          where [lhs, opr, rhs, _, dst] = words s

apply = \case AND -> (&&); OR -> (||); XOR -> (/=)


eval1 init circuit output = L.mapAccumL readOut state output
  where state = Map.fromList init
        readOut st s =
          case st Map.!? s of
            Just v -> (st, v)
            Nothing -> let (lhs, op, rhs) = circuit Map.! s
                           (st1, v1) = readOut st lhs
                           (st2, v2) = readOut st1 rhs
                           v3 = apply op v1 v2
                       in  (Map.insert s v3 st2, v3)

fromBits :: [Bool] -> Int
fromBits = L.foldr (\b acc -> acc * 2 + fromEnum b) 0

part1 :: Input -> Int
part1 (init, circuit) = fromBits $ snd $ eval1 init circuit output
  where output = filter ("z" `L.isPrefixOf`) $ Map.keys circuit
answer1 = part1 <$> input

allPairs8 keys = [ ((k1, k2), (k3, k4), (k5, k6), (k7, k8))
                 | k1 : k2s <- L.tails keys
                 , k2 : k3s <- L.tails k2s
                 , k3 : k4s <- L.tails k3s
                 , k4 : k5s <- L.tails k4s
                 , k5 : k6s <- L.tails k5s
                 , k6 : k7s <- L.tails k6s
                 , k7 : k8s <- L.tails k7s
                 , k8 <- k8s
                 ]

part2 :: Input -> Int
part2 (init, circuit) = undefined
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
--  print $ part2 inp
