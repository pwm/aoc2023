module AoC.Puzzles.Y2023D21 where

import AoC.Lib.Grid
import AoC.Lib.Parser
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

parse :: String -> Maybe Grid
parse = parseGrid parseCell

-- 3853
solveA :: Grid -> Int
solveA = Set.size . stepsN 64

-- >>> Math.NumberTheory.Primes.factorise (26501365 :: Int)
-- [(Prime 5,1),(Prime 11,1),(Prime 481843,1)]
solveB :: Grid -> ()
solveB _ = () -- 26501365 steps

stepsN :: Int -> Grid -> Set Pos
stepsN till g = go 0 (Set.singleton (startPos g))
  where
    go :: Int -> Set Pos -> Set Pos
    go n ps
      | n == till = ps
      | otherwise = go (n + 1) (steps1 g ps)

steps1 :: Grid -> Set Pos -> Set Pos
steps1 g = Set.unions . Set.map (step1 g)

step1 :: Grid -> Pos -> Set Pos
step1 g p = Set.fromList $ map fst $ filter ((/= Rock) . snd) $ lookupKNs4 p g

startPos :: Grid -> Pos
startPos = fst . Map.findMin . Map.filter (== Start)

type Grid = GridOf Cell

data Cell = Dot | Rock | Start
  deriving stock (Eq, Ord, Show)

parseCell :: Char -> Maybe Cell
parseCell = \case
  '.' -> Just Dot
  '#' -> Just Rock
  'S' -> Just Start
  _ -> Nothing

---------------------------------------------------------------------------
-- https://adventofcode.com/2023/day/21

ppg :: Grid -> IO ()
ppg = putStrLn . printGrid (\case Dot -> "."; Rock -> "#"; Start -> "S")

p :: String -> Grid
p = fromJust . parse

s0 :: String
s0 =
  unpack
    [trimming|
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
|]

ss :: String
ss = unsafePerformIO $ loadDate 2023 21
{-# NOINLINE ss #-}
