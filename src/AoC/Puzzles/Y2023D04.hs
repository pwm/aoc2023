module AoC.Puzzles.Y2023D04 where

import AoC.Lib.Parser
import AoC.Lib.Prelude hiding (id, (!))

parse :: String -> Maybe [Int]
parse = parseMaybe cardsP

solveA :: [Int] -> Int
solveA = sum . map score

solveB :: [Int] -> Int
solveB = sum . solve

solve :: [Int] -> [Int]
solve [] = []
solve (x : xs) =
  let solved = solve xs
   in 1 + sum (take x solved) : solved

score :: Int -> Int
score 0 = 0
score n = 2 ^ (n - 1)

cardsP :: Parser [Int]
cardsP = cardP `sepEndBy` newline

cardP :: Parser Int
cardP = do
  winning <- strP "Card" *> intP *> strP ":" *> some intP <* strP "|"
  mine <- some intP
  pure $ length (winning `intersect` mine)
