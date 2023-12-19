module AoC.Puzzles.Y2023D18 where

import AoC.Lib.Graph
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
-- https://adventofcode.com/2023/day/18

p :: String -> ()
p = fromJust . parse

s0 :: String
s0 =
  unpack
    [trimming|
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
|]

ss :: String
ss = unsafePerformIO $ loadDate 2023 18
{-# NOINLINE ss #-}
