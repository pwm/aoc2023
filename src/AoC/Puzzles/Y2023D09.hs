module AoC.Puzzles.Y2023D09 where

import AoC.Lib.Prelude

parse :: String -> Maybe [[Int]]
parse = Just . map (map read . splitOn " ") . lines

solveA :: [[Int]] -> Int
solveA = sum . map (sum . map last . diffs)

solveB :: [[Int]] -> Int
solveB = sum . map (foldl1' subtract . map head . diffs)

diffs :: [Int] -> [[Int]]
diffs = go []
  where
    go acc xs
      | all (0 ==) xs = xs : acc
      | otherwise = go (xs : acc) (zipWith subtract xs (tail xs))
