module AoC.Puzzles.Y2023D22 where

import AoC.Lib.Parser
import AoC.Lib.Prelude

parse :: String -> Maybe [(P3, P3)]
parse = parseMaybe (lineP `sepEndBy` newline)

solveA :: [(P3, P3)] -> Int
solveA = numCanRemove . falls . sort3z

solveB :: [(P3, P3)] -> Int
solveB = numFalls . falls . sort3z

numFalls :: [(P3, P3)] -> Int
numFalls tower = foldl' (\acc b -> acc + numFalls1 (delete b tower)) 0 tower
  where
    numFalls1 :: [(P3, P3)] -> Int
    numFalls1 bs = length $ filter id $ zipWith (/=) bs $ falls bs

numCanRemove :: [(P3, P3)] -> Int
numCanRemove tower = foldl' sumRemove 0 tower
  where
    sumRemove :: Int -> (P3, P3) -> Int
    sumRemove acc brick
      | bs <- delete brick tower, falls bs == bs = acc + 1
      | otherwise = acc

falls :: [(P3, P3)] -> [(P3, P3)]
falls = reverse . foldl' (\below brick -> fall below brick : below) []

fall :: [(P3, P3)] -> (P3, P3) -> (P3, P3)
fall below = go
  where
    go :: (P3, P3) -> (P3, P3)
    go brick
      | settled below brick = brick
      | otherwise = go (lower 1 brick)

settled :: [(P3, P3)] -> (P3, P3) -> Bool
settled below brick = min3z brick == 1 || any (touching brick) below

touching :: (P3, P3) -> (P3, P3) -> Bool
touching
  ((p1x1, p1y1, p1z1), (p1x2, p1y2, p1z2))
  ((p2x1, p2y1, p2z1), (p2x2, p2y2, p2z2)) = onTopZ && overlapXY
    where
      onTopZ = min p1z1 p1z2 - 1 == max p2z1 p2z2
      overlapXY =
        and
          [ min p1x1 p1x2 <= max p2x1 p2x2,
            max p1x1 p1x2 >= min p2x1 p2x2,
            min p1y1 p1y2 <= max p2y1 p2y2,
            max p1y1 p1y2 >= min p2y1 p2y2
          ]

lower :: Int -> (P3, P3) -> (P3, P3)
lower n ((a, b, c), (x, y, z)) = ((a, b, c - n), (x, y, z - n))

sort3z :: [(P3, P3)] -> [(P3, P3)]
sort3z = sortOn min3z

min3z :: (P3, P3) -> Int
min3z ((_, _, c), (_, _, z)) = c `min` z

type P3 = (Int, Int, Int)

lineP :: Parser (P3, P3)
lineP = liftA2 (,) (p3P <* char '~') p3P

p3P :: Parser P3
p3P = liftA3 (,,) (intP <* char ',') (intP <* char ',') intP
