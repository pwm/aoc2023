module AoC.Puzzles.Y2023D03 where

import AoC.Lib.Grid
import AoC.Lib.Parser
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map

parse :: String -> Maybe Grid
parse = parseGrid Just

solveA :: Grid -> Int
solveA g = sum $ map (extractNum g) $ partNumbers g

solveB :: Grid -> Int
solveB g = sum $ mapMaybe (ratio g (partNumbers g)) $ stars g

type Grid = GridOf Char

extractNum :: Grid -> [Pos] -> Int
extractNum g = read . lookups g

ratio :: Grid -> [[Pos]] -> Pos -> Maybe Int
ratio g numPs p
  | length touchings == 2 = Just $ product $ map (extractNum g) touchings
  | otherwise = Nothing
  where
    touchings = filter (\ps -> p `elem` numBorders ps) numPs

stars :: Grid -> [Pos]
stars = Map.keys . Map.filter (== '*')

partNumbers :: Grid -> [[Pos]]
partNumbers g =
  filter
    (any (`notElem` ('.' : ['0' .. '9'])) . lookups g . numBorders)
    (idNums g)

idNums :: Grid -> [[Pos]]
idNums g = snd $ Map.foldrWithKey f ([], []) numPs
  where
    numPs = Map.filter (`elem` ['0' .. '9']) g
    f :: Pos -> Char -> ([Pos], [[Pos]]) -> ([Pos], [[Pos]])
    f digit _ (digits, nums) =
      if digit <-> (0, 1) `Map.member` numPs
        then (digit : digits, nums)
        else ([], (digit : digits) : nums)

numBorders :: [Pos] -> [Pos]
numBorders xs = nubOrd (concatMap adj8 xs) \\ xs
