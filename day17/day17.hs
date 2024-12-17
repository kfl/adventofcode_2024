{-# LANGUAGE LambdaCase, DuplicateRecordFields, OverloadedRecordDot #-}
module Main where

import qualified Data.List as L

import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))
import Data.Bits(xor)

import qualified Data.SBV as S
import Data.SBV ( (.==), (.&&), (.&.) )

test =  parse [ "Register A: 729"
              , "Register B: 0"
              , "Register C: 0"
              , ""
              , "Program: 0,1,5,4,3,0"
              ]
input = parse . lines <$> readFile "input.txt"

type Prog = V.Vector Int
type Input = (Int, Int, Int, Prog)

parse :: [String] -> Input
parse [rega, regb, regc, _, prog] = (readReg rega, readReg regb, readReg regc, readProg prog)
  where readReg = read . drop (length "Register X: ")
        readProg s = read $ "[" ++ drop (length "Program: ") s ++ "]"

data Combo = Lit Int
           | RegA
           | RegB
           | RegC
  deriving (Eq, Show)

data Instr = Adv Combo
           | Bxl Int
           | Bst Combo
           | Jnz Int
           | Bxc
           | Out Combo
           | Bdv Combo
           | Cdv Combo
  deriving (Eq, Show)

type Pc = Int

combo = \case
  4 -> RegA
  5 -> RegB
  6 -> RegC
  n -> Lit n

decode :: Prog -> Pc -> Instr
decode prog pc =
  case prog ! pc of
    0 -> Adv $ combo next
    1 -> Bxl next
    2 -> Bst $ combo next
    3 -> Jnz next
    4 -> Bxc
    5 -> Out $ combo next
    6 -> Bdv $ combo next
    7 -> Cdv $ combo next
  where
    next = prog ! (pc+1)

data State num = State { rega, regb, regc :: num, pc :: Int, out :: [Int] }
  deriving (Eq, Show)

run prog st | st.pc >= V.length prog = st.out
run prog st =
  case instr of
    Adv c -> run prog st{rega = st.rega `div` 2^cval c, pc = st.pc + 2}
    Bxl x -> run prog st{regb = st.regb `xor` x, pc = st.pc + 2}
    Bst c -> run prog st{regb = cval c `mod` 8, pc = st.pc + 2}
    Jnz x -> run prog st{pc = if st.rega == 0 then st.pc + 2 else x}
    Bxc   -> run prog st{regb = st.regb `xor` st.regc, pc = st.pc + 2}
    Out c -> run prog st{out = cval c `mod` 8 : st.out, pc = st.pc + 2}
    Bdv c -> run prog st{regb = st.rega `div` 2^cval c, pc = st.pc + 2}
    Cdv c -> run prog st{regc = st.rega `div` 2^cval c, pc = st.pc + 2}
  where
    instr = decode prog st.pc
    cval = \case
      Lit n -> n
      RegA -> st.rega
      RegB -> st.regb
      RegC -> st.regc

part1 :: Input -> String
part1 (a, b, c, prog) = L.intercalate "," $ map show $ reverse $ run prog initial
  where initial = State a b c 0 []
answer1 = part1 <$> input

type Symbolic = State S.SWord64

symbolic :: Prog -> Symbolic -> S.SBool
symbolic prog st | st.pc >= V.length prog = S.fromBool $ null st.out
symbolic prog st =
  case instr of
    Adv c -> symbolic prog st{rega = st.rega `S.sShiftRight` cval c, pc = st.pc + 2}
    Bxl x -> symbolic prog st{regb = st.regb `S.xor` fromIntegral x, pc = st.pc + 2}
    Bst c -> symbolic prog st{regb = cval c .&. 7, pc = st.pc + 2}
    Bxc   -> symbolic prog st{regb = st.regb `S.xor` st.regc, pc = st.pc + 2}
    Bdv c -> symbolic prog st{regb = st.rega `S.sShiftRight` cval c, pc = st.pc + 2}
    Cdv c -> symbolic prog st{regc = st.rega `S.sShiftRight` cval c, pc = st.pc + 2}

    Out c -> case st.out of
               [] -> S.sFalse
               x:xs -> cval c .&. 7 .== fromIntegral x .&&
                       symbolic prog st{out = xs, pc = st.pc + 2}
    Jnz x -> S.symbolicMerge False (st.rega .== 0)
                             (symbolic prog st{pc = st.pc + 2})
                             (symbolic prog st{pc = x})
  where
    instr = decode prog st.pc
    cval = \case
      Lit n -> fromIntegral n
      RegA -> st.rega
      RegB -> st.regb
      RegC -> st.regc

part2 :: Input -> IO S.Word64
part2 (a, b, c, prog) = do
  solution <- S.optLexicographic $ do
    initial <- S.free "initial"
    S.minimize "smallest" initial
    S.constrain $ symbolic prog $ State initial (fromIntegral b) (fromIntegral c) 0 $ V.toList prog
  case S.getModelValue "initial" solution of
    Just n -> pure n
    Nothing -> error "No solution"

answer2 = part2 =<< input

main = do
  inp <- input
  putStrLn $ part1 inp
  print =<< part2 inp
