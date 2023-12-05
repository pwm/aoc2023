module AoC.Puzzles.Y2023D05 where

import AoC.Lib.Parser
import AoC.Lib.Prelude

parse :: String -> Maybe ([Int], [Layer])
parse = parseMaybe almanacP

solveA :: ([Int], [Layer]) -> Int
solveA (seeds, almanac) = minimum $ map (`bottom` almanac) seeds

-- todo: this is dumb brute-force taking 10s, come up with a better solution
solveB :: ([Int], [Layer]) -> Int
solveB (seeds, almanac) =
  let seedRanges = fromJust $ traverse l2p $ chunksOf 2 seeds
      lowest = head $ filter (isSeedId seedRanges) $ map (`top` almanac) [0 ..]
   in bottom lowest almanac

type DSR = (Int, Int, Int)

type Layer = [DSR]

isSeedId :: [(Int, Int)] -> Int -> Bool
isSeedId seeds n = any (\(s, r) -> s <= n && n < s + r) seeds

bottom, top :: Int -> [Layer] -> Int
bottom = foldl' mapDown
top = foldr (flip mapUp)

mapDown :: Int -> Layer -> Int
mapDown n [] = n
mapDown n (x : xs) =
  let n' = dest n x
   in if n /= n' then n' else mapDown n xs

mapUp :: Int -> Layer -> Int
mapUp n [] = n
mapUp n (x : xs) =
  let n' = source n x
   in if n /= n' then n' else mapUp n xs

dest :: Int -> DSR -> Int
dest n (d, s, r)
  | s <= n && n < s + r = d + n - s
  | otherwise = n

source :: Int -> DSR -> Int
source n (d, s, r) = dest n (s, d, r)

almanacP :: Parser ([Int], [Layer])
almanacP = liftA2 (,) (seedP <* newline) (layerP `sepEndBy` newline)

seedP :: Parser [Int]
seedP = strP "seeds:" *> some intP <* newline

layerP :: Parser Layer
layerP = do
  _ <- some (letterChar <|> char '-' <|> char ':' <|> char ' ') <* newline
  liftA3 (,,) intP intP intP `sepEndBy` newline
