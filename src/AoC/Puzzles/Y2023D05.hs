module AoC.Puzzles.Y2023D05 where

import AoC.Lib.Interval
import AoC.Lib.Parser
import AoC.Lib.Prelude

parse :: String -> Maybe ([Int], [Layer])
parse = parseMaybe almanacP

solveA :: ([Int], [Layer]) -> Int
solveA (seeds, layers) =
  minimum $ map (`downAll` layers) seeds

solveB :: ([Int], [Layer]) -> Int
solveB (seeds, layers) =
  let seedRanges = catMaybes [mkIV (a, a + b) | [a, b] <- chunksOf 2 seeds]
   in (.lo) $ minimumBy (comparing (.lo)) $ concatMap (`downAllIV` layers) seedRanges

type Layer = [(Int, Int, Int)]

downAllIV :: IV Int -> [Layer] -> [IV Int]
downAllIV = foldlM down1IV

down1IV :: IV Int -> Layer -> [IV Int]
down1IV iv layer =
  let (cuts, offsets) = unzip $ mapMaybe (cut iv) layer
   in iunions $ zipWith (<+>) cuts offsets <> foldlM idiff iv cuts

cut :: IV Int -> (Int, Int, Int) -> Maybe (IV Int, Int)
cut iv (d, s, r)
  | Just (IV a b) <- iv `icut` IV s (s + r) = Just (IV a b, d - s)
  | otherwise = Nothing

downAll :: Int -> [Layer] -> Int
downAll = foldl' down1

down1 :: Int -> Layer -> Int
down1 n [] = n
down1 n ((d, s, r) : dsrs)
  | s <= n && n < s + r = d + n - s
  | otherwise = down1 n dsrs

almanacP :: Parser ([Int], [Layer])
almanacP = liftA2 (,) (seedsP <* newline) (layerP `sepEndBy` newline)

seedsP :: Parser [Int]
seedsP = strP "seeds:" *> some intP <* newline

layerP :: Parser Layer
layerP = do
  _ <- some (letterChar <|> char '-' <|> char ':' <|> char ' ') <* newline
  liftA3 (,,) intP intP intP `sepEndBy` newline
