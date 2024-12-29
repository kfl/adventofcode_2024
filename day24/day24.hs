{-# LANGUAGE LambdaCase, Strict #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Data.Bifunctor (second)

import Data.Maybe (maybeToList)
import Text.Printf
import System.Process (callCommand)


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

eval1 init circuit = map read
  where state = Map.fromList init
                <> fmap (\(lhs, op, rhs) -> apply op (read lhs) (read rhs)) circuit
        read s = state Map.! s

fromBits :: [Bool] -> Int
fromBits = L.foldr (\b acc -> acc * 2 + fromEnum b) 0

part1 :: Input -> Int
part1 (init, circuit) = fromBits $ eval1 init circuit output
  where output = filter ("z" `L.isPrefixOf`) $ Map.keys circuit
answer1 = part1 <$> input



showCircuit name circuit = do
  let trans = [ s
              | (cur, (lhs, op, rhs)) <- Map.assocs circuit
              , let opr = lhs++show op++rhs
                    colour
                      | "z" `L.isPrefixOf` cur && cur /= "z45" && op /= XOR = ["orange"]
                      | otherwise = [ "red"
                                    | prev <- [lhs, rhs]
                                    , case circuit Map.!? prev of
                                        Just (_, prevOp, _) ->
                                          case (prevOp, op) of
                                            (AND, AND) -> True -- AND gates should not connect
                                            (OR, OR)   -> True -- OR gates should not connect
                                            (XOR, OR)  -> True
                                            _ -> False
                                        _ -> False ]
              , s <- [ printf "  %s [label=\"%s\", style=solid];" opr (show op)
                     , printf "  %s -> %s;" lhs opr
                     , printf "  %s -> %s;" rhs opr
                     , printf "  %s -> %s;" opr cur] ++
                     [ printf "  %s [color=\"%s\"];" cur c | c <- take 1 colour ]
              ]
      dotfile = name ++ ".dot"
      pdffile = name ++ ".pdf"
      pre = [ "digraph circuit {"
            , "   rankdir=\"LR\";"
            , "   node [style=filled];"
            ]
  writeFile dotfile $ unlines $ pre ++ trans ++ ["}"]
  callCommand $ "dot -Tpdf "++dotfile ++" -o "++pdffile
  callCommand $ "open -a Preview "++pdffile


swap circuit (x,y) = Map.insert x (circuit Map.! y) $ Map.insert y (circuit Map.! x) circuit
swaps xys circuit = L.foldl' swap circuit xys


debugViaManualLabour = do
   -- showCircuit "orig" =<< swaps [] . snd <$> input
   showCircuit "fixed"
     . swaps [("jss", "rds"), ("mvb", "z08"),("wss", "z18"), ("bmn", "z23")]
     . snd
     =<< input
part2 :: Input -> String
part2 (init, circuit) =  L.intercalate ","
                         $ L.sort
                         $ concatMap (\(x,y) -> [x,y]) [ ("jss", "rds"), ("mvb", "z08")
                                                       , ("wss", "z18"), ("bmn", "z23")]
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  putStrLn $ part2 inp
