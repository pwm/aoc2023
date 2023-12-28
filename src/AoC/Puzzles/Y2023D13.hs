module AoC.Puzzles.Y2023D13 where

import AoC.Lib.Prelude
import Control.Lens (Ixed (ix))

parse :: String -> Maybe [[String]]
parse = Just . splitOn [""] . lines

solveA :: [[String]] -> Int
solveA = sum . map (score . refl)

solveB :: [[String]] -> Int
solveB = sum . map (score . cleanRefl)

data C = C {hors :: [Int], vers :: [Int]}
  deriving stock (Eq, Ord, Show)

score :: C -> Int
score c = 100 * sum c.hors + sum c.vers

cleanRefl :: [String] -> C
cleanRefl m = head $ valids $ map (`cdiff` refl m) (refls m)

cdiff :: C -> C -> C
cdiff (C a b) (C c d) = C (a \\ c) (b \\ d)

refls :: [String] -> [C]
refls m = valids $ do
  v <- [0 .. length m - 1]
  h <- [0 .. length (m !! v) - 1]
  pure $ refl $ m & ix v . ix h %~ (\case '.' -> '#'; _ -> '.')

valids :: [C] -> [C]
valids = nubOrd . filter (/= C [] [])

refl :: [String] -> C
refl m0 = C (go 1 m0) (go 1 (transpose m0))
  where
    go :: Int -> [String] -> [Int]
    go n m
      | n < length m,
        (a, b) <- splitAt n m,
        b' <- take n b,
        a' <- takeEnd (length b') a,
        a' == reverse b' =
          n : go (n + 1) m
      | n < length m = go (n + 1) m
      | otherwise = []
