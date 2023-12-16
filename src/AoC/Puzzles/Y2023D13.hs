module AoC.Puzzles.Y2023D13 where

import AoC.Lib.Grid
import AoC.Lib.Parser
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

parse :: String -> Maybe [[String]]
parse = Just . splitOn [""] . lines

solveA :: a -> ()
solveA _ = ()

solveB :: a -> ()
solveB _ = ()

rotcw, rotacw :: Int -> [[a]] -> [[a]]
rotcw n = times n (map reverse . transpose)
rotacw n = times n (reverse . transpose)

row, col :: [[a]] -> Int
row = length
col = length . head

{-
start from col 1 to length - 2
  expand left-right till reaching the end on one side

-}

---------------------------------------------------------------------------
-- Test data

p :: String -> [[String]]
p = fromJust . parse

s0 :: String
s0 =
  unpack
    [trimming|
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
|]

ss :: String
ss = unsafePerformIO $ loadDate 2023 13
{-# NOINLINE ss #-}
