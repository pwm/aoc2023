module AoC.Puzzles.Y2023D01 where

import AoC.Lib.Parser
import AoC.Lib.Prelude

parse :: String -> Maybe [String]
parse = Just . lines

solveA :: [String] -> Int
solveA = maybe 0 sum . traverse (stringToInt . ends . filter isDigit)

solveB :: [String] -> Int
solveB = maybe 0 sum . traverse (digitsToInt . ends) . mapMaybe digitify

ends :: [a] -> [a]
ends s = take 1 s <> takeEnd 1 s

digitify :: String -> Maybe [Int]
digitify = parseMaybe (catMaybes <$> some (digitP <|> digitTextP))

digitP, digitTextP :: Parser (Maybe Int)
digitP = Just . digitToInt <$> digitChar
digitTextP = optional (lookAhead t2dP) <* alphaNumChar

t2dP :: Parser Int
t2dP =
  choice
    [ 1 <$ string "one",
      2 <$ string "two",
      3 <$ string "three",
      4 <$ string "four",
      5 <$ string "five",
      6 <$ string "six",
      7 <$ string "seven",
      8 <$ string "eight",
      9 <$ string "nine"
    ]
