module Main where

import qualified Data.Char as C
import qualified Data.List as L
import Text.ParserCombinators.ReadP


test =  parse "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
input = parse <$> readFile "input.txt"

type Multiplication = (Int, Int)
type Input = [Multiplication]

parse :: String -> Input
parse str = res
  where [(res, _)] = readP_to_S (munchMany $ skipTill anyc mult) str

digit = satisfy C.isDigit
anyc = satisfy $ const True
num :: ReadP Int
num = read <$> choice [ count i digit | i <- [1..3] ]
mult = (,) <$> (string "mul(" *> num <* char ',') <*> (num <* char ')')

munchMany p = ((:) <$> p <*> munchMany p) <++ return []

skipTill p end = scan
  where scan = end <++ (p *> scan)

part1 :: Input -> Int
part1 input = sum $ map (uncurry (*)) input
answer1 = part1 <$> input


data Inst = Mult Int Int
          | Do
          | Dont
          deriving (Eq, Show)
type Input2 = [Inst]

test2 =  parse2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
input2 = parse2 <$> readFile "input.txt"

parse2 :: String -> Input2
parse2 str = res
  where
    inst = choice [Do <$ string "do()", Dont <$ string "don't()", uncurry Mult <$> mult]
    [(res, _)] = readP_to_S (munchMany $ skipTill anyc inst) str

part2 :: Input2 -> Int
part2 input = snd $ L.foldl' step (True, 0) input
  where step (_, acc)       Do         = (True, acc)
        step (_, acc)       Dont       = (False, acc)
        step acc@(False, _) _          = acc
        step (enabled, acc) (Mult x y) = (enabled, acc + x * y)
  
answer2 = part2 <$> input2

main = do
  answer1 >>= print
  answer2 >>= print
