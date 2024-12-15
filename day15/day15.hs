{-# LANGUAGE LambdaCase, Strict #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import qualified Data.Array.Unboxed as U
import Data.Array.Unboxed ((!))
import qualified Data.Array.ST as M
import Data.Array.ST ()

import Control.Monad (forM_, foldM)
import Data.Maybe (mapMaybe)


test  = parse $ unlines [ "##########"
                        , "#..O..O.O#"
                        , "#......O.#"
                        , "#.OO..O.O#"
                        , "#..O@..O.#"
                        , "#O#..O...#"
                        , "#O..O..O.#"
                        , "#.OO.O.OO#"
                        , "#....O...#"
                        , "##########"
                        , ""
                        , "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^"
                        , "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v"
                        , "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<"
                        , "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^"
                        , "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><"
                        , "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^"
                        , ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^"
                        , "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>"
                        , "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>"
                        , "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
                        ]
smallTest = parse $ unlines [ "########"
                            , "#..O.O.#"
                            , "##@.O..#"
                            , "#...O..#"
                            , "#.#.O..#"
                            , "#...O..#"
                            , "#......#"
                            , "########"
                            , ""
                            , "<^^>>>vv<v>>v<<"
                            ]
input = parse <$> readFile "input.txt"

type Row = Int
type Col = Int
type Pos = (Col, Row)
type Dir = (Col, Row)

type Grid = U.UArray Pos Char
type MGrid = M.STUArray Pos Char
type Input = (Grid, [Dir])


parseGrid :: [String] -> Grid
parseGrid rows@(cols : _) = U.array bounds letters
  where letters = [ ((x,y), c) | (y, row) <- zip [0..] rows
                               , (x, c) <- zip [0..] row]
        bounds = ((0,0), (length cols - 1, length rows - 1))

parse :: String -> Input
parse str = (parseGrid $ lines grid,
             mapMaybe (\case '^' -> Just north
                             'v' -> Just south
                             '>' -> Just east
                             '<' -> Just west
                             _   -> Nothing) moves)
  where [grid, moves] = L.splitOn "\n\n" str

directions@[north, south, east, west] = [ (0,-1), (0,1), (1,0), (-1,0) ]
(a, b) |+| (c, d) = (a+c, b+d)

getFree d p grid = do
  c <- M.readArray grid p
  case c of
    '#' -> pure Nothing
    '.' -> pure $ Just p
    'O' -> getFree d (p |+| d) grid

move grid cur d = do
  let next = cur |+| d
  p <- getFree d next grid
  case p of
    Just p -> do
      M.writeArray grid p 'O'
      M.writeArray grid next '@'
      M.writeArray grid cur '.'
      return next
    _ -> pure cur

getRobotPos grid = maybe (-1,-1) fst $ L.find ((== '@') . snd) $ U.assocs grid

doMoves igrid moves = M.runSTUArray $ do
  let cur = getRobotPos igrid
  grid <- M.thaw igrid
  after <- foldM (move grid) cur moves
  return grid

showGrid grid = do
  forM_ [miny .. maxy] $ \y -> do
    forM_ [minx .. maxx] $ \x -> do
       putStr [grid ! (x,y)]
    putStrLn ""
  where ((minx, miny), (maxx, maxy)) = U.bounds grid

gps count grid = sum [ 100*y + x | ((x,y), c) <- grid, c == count]

part1 :: Input -> Int
part1 input@(grid, moves) = gps 'O' $  U.assocs $ doMoves grid moves
answer1 = part1 <$> input


smallTest2 = parse $ unlines [ "#######"
                             , "#...#.#"
                             , "#.....#"
                             , "#..OO@#"
                             , "#..O..#"
                             , "#.....#"
                             , "#######"
                             , ""
                             , "<vv<<^^<<^^"
                             ]

scaleUp :: Grid -> Grid
scaleUp grid = U.array (lower, (2*maxx+1, maxy)) [ e | ((x,y), c) <- U.assocs grid
                                                     , let [c1, c2] = case c of
                                                                        '@' -> "@."
                                                                        'O' -> "[]"
                                                                        _   -> [c,c]
                                                     , e <- [ ((2*x, y), c1), ((2*x+1, y), c2) ]]
  where (lower, (maxx, maxy)) = U.bounds grid

tryMoveH d to p grid =
  case Map.lookup p grid of
    Just '#' -> Nothing
    Just c   -> Map.insert p to <$> tryMoveH d c (p |+| d) grid
    Nothing  -> Just $ Map.insert p to grid

tryMoveV _ _ toMove [] = Just toMove
tryMoveV d grid toMove (p : worklist)
  | Map.notMember p toMove , Just c <- Map.lookup p grid =
      if c == '#' then Nothing
      else tryMoveV d grid (Map.insert p c toMove) $ (case c of
                                                        '[' -> [p |+| east, p |+| d]
                                                        ']' -> [p |+| west, p |+| d]
                                                        '@' -> [p |+| d]
                                                        _   -> []) ++ worklist
  | otherwise = tryMoveV d grid toMove worklist

move2 acc@(grid, cur) d =
  case d of
    (_, 0) -> maybe acc ((, cur |+| d) . Map.delete cur) $ tryMoveH d '@' (cur |+| d) grid
    _ -> case tryMoveV d grid Map.empty [cur] of
           Nothing -> acc
           Just toMove -> (updated, cur |+| d)
             where updated = Map.union (Map.mapKeysMonotonic (|+| d) toMove)
                                       (Map.difference grid toMove)

part2 :: Input -> Int
part2 input@(grid, moves) = gps '[' $ Map.assocs mgrid'
  where scaled = scaleUp grid
        cur = getRobotPos scaled
        mgrid = Map.fromList [ e | e@(p, c) <- U.assocs scaled, c /= '.' ]
        (mgrid', _) = L.foldl' move2 (mgrid, cur) moves

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
