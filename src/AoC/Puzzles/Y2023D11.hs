module AoC.Puzzles.Y2023D11 where

import AoC.Lib.Grid
import AoC.Lib.Parser
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

parse :: String -> Maybe Grid
parse = parseGrid parseCell

solveA :: Grid -> Int
solveA = solve . expand 2

solveB :: Grid -> Int
solveB = solve . expand 1000000

solve :: Grid -> Int
solve = sum . map (uncurry manhattan) . mapMaybe l2p . pick 2 . Map.keys

expand :: Int -> Grid -> Grid
expand n g = Map.foldlWithKey' expandPos mempty $ Map.filter (== On) g
  where
    (er, ec) = emptyRowsCols g
    expandPos :: Grid -> Pos -> Cell -> Grid
    expandPos acc (x, y) _ =
      let growBy a = length . filter (< a)
       in Map.insert (growBy x er * (n - 1) + x, growBy y ec * (n - 1) + y) On acc

emptyRowsCols :: Grid -> ([Int], [Int])
emptyRowsCols g = (emptyRows, emptyCols)
  where
    (vMin, hMin) = fst $ Map.findMin g
    (vMax, hMax) = fst $ Map.findMax g
    rowOf x = [(x, v) | v <- [hMin .. hMax]]
    colOf x = [(h, x) | h <- [vMin .. vMax]]
    emptyRows = filter (emptyLine g . rowOf) [vMin .. vMax]
    emptyCols = filter (emptyLine g . colOf) [hMin .. hMax]

emptyLine :: Grid -> [Pos] -> Bool
emptyLine g = null . Map.filter (== On) . Map.restrictKeys g . Set.fromList

type Grid = GridOf Cell

data Cell = Off | On
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

parseCell :: Char -> Maybe Cell
parseCell = \case
  '.' -> Just Off
  '#' -> Just On
  _ -> Nothing
