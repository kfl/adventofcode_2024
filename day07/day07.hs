{-# LANGUAGE LambdaCase #-}
module Main where

test =  map parse [ "190: 10 19"
                  , "3267: 81 40 27"
                  , "83: 17 5"
                  , "156: 15 6"
                  , "7290: 6 8 6 15"
                  , "161011: 16 10 13"
                  , "192: 17 8 14"
                  , "21037: 9 7 18 13"
                  , "292: 11 6 16 20"
                  ]
input = map parse . lines <$> readFile "input.txt"

type Equation = (Int, [Int])
type Input = [Equation]

parse :: String -> Equation
parse str = (read result, map read $ words $ drop 1 constants)
  where (result, constants) = break (== ':') str

canEvaluateTo acc = \case
  [] -> [acc]
  x:xs -> canEvaluateTo (acc + x) xs ++ canEvaluateTo (acc * x) xs

part1 :: Input -> Int
part1 input = sum $ map fst possibles
  where possible (res, xs) = elem res $ canEvaluateTo 0 xs
        possibles = filter possible input

answer1 = part1 <$> input

numDigitsSlow x = ceiling $ logBase 10 (fromIntegral (x + 1))
conc x y = x * 10 ^ (numDigits y) + y
  where numDigits y =
          if y < 10 then 1
          else if y < 100 then 2
          else if y < 1000 then 3
          else if y < 10000 then 4
          else 5

canEvaluateTo2 max !acc = \case
  _ | acc > max -> []
  [] -> [acc]
  x:xs -> canEvaluateTo2 max (acc + x) xs ++ canEvaluateTo2 max (acc * x) xs ++
          canEvaluateTo2 max (conc acc x) xs

part2 :: Input -> Int
part2 input = sum $ map fst possibles
  where possible (res, xs) = elem res $ canEvaluateTo2 res 0 xs
        possibles = filter possible input

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
