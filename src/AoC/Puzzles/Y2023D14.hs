module AoC.Puzzles.Y2023D14 where

import AoC.Lib.Grid
import AoC.Lib.Parser
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

parse :: String -> Maybe Grid
parse = parseGrid parseCell

-- 106378
solveA :: Grid -> Int
solveA = totalLoad . rolls U

solveB :: a -> ()
solveB _ = ()

{-
1_000_000_000
-}

totalLoad :: Grid -> Int
totalLoad g =
  let (vMax, _) = bimap (+ 1) (+ 1) $ fst $ Map.findMax g
   in sum $ map (\(v, _) -> vMax - v) $ rounds g

rolls :: Dir4 -> Grid -> Grid
rolls dir = fixpoint (roll1 dir)

roll1 :: Dir4 -> Grid -> Grid
roll1 dir g0 = foldl' go g0 (rounds g0)
  where
    go :: Grid -> Pos -> Grid
    go g p =
      let p' = step4 p dir
       in case g !? p' of
            Just Dot -> Map.insert p' Round (Map.insert p Dot g)
            _ -> g

rounds :: Grid -> [Pos]
rounds = Map.keys . Map.filter (== Round)

type Grid = GridOf Cell

data Cell = Dot | Square | Round
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

parseCell :: Char -> Maybe Cell
parseCell = \case
  '.' -> Just Dot
  '#' -> Just Square
  'O' -> Just Round
  _ -> Nothing

ppCell :: Cell -> String
ppCell = \case
  Dot -> "."
  Square -> "#"
  Round -> "O"

ppg :: Grid -> String
ppg = printGrid ppCell

ddg :: Grid -> IO ()
ddg = putStrLn . ppg

---------------------------------------------------------------------------
-- Test data

p :: String -> Grid
p = fromJust . parse

s0 :: String
s0 =
  unpack
    [trimming|
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....

|]

ss :: String
ss = unsafePerformIO $ loadDate 2023 14
{-# NOINLINE ss #-}
