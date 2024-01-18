module AoC.Puzzles.Y2023D12 where

import AoC.Lib.Memo
import AoC.Lib.Prelude

parse :: String -> Maybe [(String, [Int])]
parse =
  traverse
    ((\(a, b) -> (a,) <$> traverse readMaybe (splitOn "," b)) <=< l2p . splitOn " ")
    . lines

solveA :: [(String, [Int])] -> Int
solveA = sum . map solve1

solveB :: [(String, [Int])] -> Int
solveB = sum . map (solve1 . extend)

solve1 :: (String, [Int]) -> Int
solve1 (pat0, clue0) = memoMap go (0, length pat, length clue)
  where
    pat = "." <> pat0 <> "."
    clue = "." <> intercalate "." [replicate n '#' | n <- clue0] <> "."
    go :: (Monad m) => ((Int, Int, Int) -> m Int) -> (Int, Int, Int) -> m Int
    go rec (acc, pIdx, cIdx)
      | pIdx == 0 && cIdx == 0 = pure 1
      | (pIdx == 0) /= (cIdx == 0) = pure 0
      | pat !! (pIdx - 1) `elem` ['#', '?'] && clue !! (cIdx - 1) == '#' = do
          r1 <- rec (acc, pIdx - 1, cIdx - 1)
          pure $ acc + r1
      | pat !! (pIdx - 1) `elem` ['.', '?'] && clue !! (cIdx - 1) == '.' = do
          r1 <- rec (acc, pIdx - 1, cIdx)
          r2 <- rec (acc, pIdx - 1, cIdx - 1)
          pure $ acc + r1 + r2
      | otherwise = pure acc

extend :: (String, [Int]) -> (String, [Int])
extend (s, xs) = (intercalate "?" (replicate 5 s), concat (replicate 5 xs))
