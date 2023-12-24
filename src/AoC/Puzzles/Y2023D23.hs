module AoC.Puzzles.Y2023D23 where

import AoC.Lib.Graph
import AoC.Lib.Grid
import AoC.Lib.Parser
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

parse :: String -> Maybe Grid
parse = parseGrid parseCell

solveA :: Grid -> ()
solveA _ = ()

solveB :: Grid -> ()
solveB _ = ()

type Grid = GridOf Cell

data Cell = Dot | Bush | AU | AR | AD | AL
  deriving stock (Show, Eq, Ord, Generic)

parseCell :: Char -> Maybe Cell
parseCell = \case
  '.' -> Just Dot
  '#' -> Just Bush
  '^' -> Just AU
  '>' -> Just AR
  'v' -> Just AD
  '<' -> Just AL
  _ -> Nothing

ppCell :: Cell -> String
ppCell = \case
  Dot -> "."
  Bush -> "#"
  AU -> "^"
  AR -> ">"
  AD -> "v"
  AL -> "<"

ppg :: Grid -> IO ()
ppg = putStrLn . printGrid ppCell

---------------------------------------------------------------------------
-- https://adventofcode.com/2023/day/23

p :: String -> Grid
p = fromJust . parse

s0 :: String
s0 =
  unpack
    [trimming|
#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#
|]

ss :: String
ss = unsafePerformIO $ loadDate 2023 23
{-# NOINLINE ss #-}
