module AoC.Puzzles.Y2023D13 where

import AoC.Lib.Grid
import AoC.Lib.Parser
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

parse :: String -> Maybe ()
parse _ = Just ()

solveA :: a -> ()
solveA _ = ()

solveB :: a -> ()
solveB _ = ()

---------------------------------------------------------------------------
-- Test data

p :: String -> ()
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
