module AoC.Puzzles.Y2023D02 where

import AoC.Lib.Parser
import AoC.Lib.Prelude

parse :: String -> Maybe [(Int, [[(RGB, Int)]])]
parse = parseMaybe (lineP `sepEndBy` newline)

solveA :: [(Int, [[(RGB, Int)]])] -> Int
solveA = sum . map fst . filter (all (all possible) . snd)

solveB :: [(Int, [[(RGB, Int)]])] -> Int
solveB = sum . map (power . minNeeded . snd)

data RGB = Red | Green | Blue
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data Max = Max {red :: Int, green :: Int, blue :: Int}
  deriving stock (Show, Eq, Ord)

possible :: (RGB, Int) -> Bool
possible (rgb, n) = case rgb of
  Red -> n <= 12
  Green -> n <= 13
  Blue -> n <= 14

minNeeded :: [[(RGB, Int)]] -> Max
minNeeded = foldl' f (Max 0 0 0) . concat
  where
    f :: Max -> (RGB, Int) -> Max
    f (Max r g b) (rgb, n) = case rgb of
      Red -> Max (max r n) g b
      Green -> Max r (max g n) b
      Blue -> Max r g (max b n)

power :: Max -> Int
power (Max r g b) = r * g * b

lineP :: Parser (Int, [[(RGB, Int)]])
lineP = do
  gameId <- strP "Game" *> intP <* strP ":"
  pulls <- ((swap <$> liftA2 (,) intP rgbP) `sepBy` strP ",") `sepBy` strP ";"
  pure (gameId, pulls)

rgbP :: Parser RGB
rgbP = enumParser (fmap toLower . show) $ \case
  "red" -> Just Red
  "green" -> Just Green
  "blue" -> Just Blue
  _ -> Nothing
