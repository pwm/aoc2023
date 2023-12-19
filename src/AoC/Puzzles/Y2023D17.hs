module AoC.Puzzles.Y2023D17 where

import AoC.Lib.Graph
import AoC.Lib.Grid
import AoC.Lib.Parser
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map
import Data.PQueue.Prio.Min (MinPQueue)
import Data.PQueue.Prio.Min qualified as MinPQueue
import Data.Sequence qualified as Seq
import Data.Set qualified as Set

parse :: String -> Maybe Grid
parse = parseGrid charToDigit

solveA :: a -> ()
solveA _ = ()

solveB :: a -> ()
solveB _ = ()

{-
The starting point, the lava pool, is the top-left city block; the destination, the machine parts factory, is the bottom-right city block.

it can move at most three blocks in a single direction before it must turn 90 degrees left or right

The crucible also can't reverse direction; after entering each city block, it may only turn left, continue straight, or turn right.

Need Dir4 otherwise it's tricky yo compute the diff between v>>> and >>>> ...
-}

type Grid = GridOf Int

---------------------------------------------------------------------------
-- https://adventofcode.com/2023/day/17

p :: String -> Grid
p = fromJust . parse

s0 :: String
s0 =
  unpack
    [trimming|
2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
|]

ss :: String
ss = unsafePerformIO $ loadDate 2023 17
{-# NOINLINE ss #-}
