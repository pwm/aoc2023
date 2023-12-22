module AoC.Puzzles.Y2023D22 where

import AoC.Lib.Parser
import AoC.Lib.Prelude

parse :: String -> Maybe [(P3, P3)]
parse = parseMaybe (lineP `sepEndBy` newline)

-- 480 (slow)
solveA :: [(P3, P3)] -> Int
solveA = numCanRemove . falls . sort3z

solveB :: [(P3, P3)] -> ()
solveB _ = ()

numCanRemove :: [(P3, P3)] -> Int
numCanRemove bs =
  let f2 :: Int -> (P3, P3) -> Int
      f2 acc b =
        -- opt: just use the subset on top + only 1 fall is enough
        let bs' = delete b bs
         in if falls bs' == bs' then acc + 1 else acc
   in foldl' f2 0 bs

falls :: [(P3, P3)] -> [(P3, P3)]
falls = reverse . foldl' fall1 []

fall1 :: [(P3, P3)] -> (P3, P3) -> [(P3, P3)]
fall1 below brick
  | touchingAny below brick || min3z brick == 1 = brick : below
  | otherwise = fall1 below (lower 1 brick) -- opt: lower max possible

-- opt: only check the top of below (lazy `any` should do that already?)
touchingAny :: [(P3, P3)] -> (P3, P3) -> Bool
touchingAny below (pa, pb) = any (touching1 (pa, pb)) below

touching1 :: (P3, P3) -> (P3, P3) -> Bool
touching1
  ((p1x1, p1y1, p1z1), (p1x2, p1y2, p1z2))
  ((p2x1, p2y1, p2z1), (p2x2, p2y2, p2z2)) = overlap && onTop
    where
      onTop = min p1z1 p1z2 - 1 == max p2z1 p2z2
      overlap = not (null (x1 `intersect` x2))
      x1 = [(a, b) | a <- [min p1x1 p1x2 .. max p1x1 p1x2], b <- [min p1y1 p1y2 .. max p1y1 p1y2]]
      x2 = [(a, b) | a <- [min p2x1 p2x2 .. max p2x1 p2x2], b <- [min p2y1 p2y2 .. max p2y1 p2y2]]

lower :: Int -> (P3, P3) -> (P3, P3)
lower n ((a, b, c), (x, y, z)) = ((a, b, c - n), (x, y, z - n))

sort3z :: [(P3, P3)] -> [(P3, P3)]
sort3z = sortOn min3z

min3z :: (P3, P3) -> Int
min3z (pa, pb) = thdOf3 pa `min` thdOf3 pb

type P3 = (Int, Int, Int)

lineP :: Parser (P3, P3)
lineP = liftA2 (,) (p3P <* char '~') p3P

p3P :: Parser P3
p3P = liftA3 (,,) (intP <* char ',') (intP <* char ',') intP

---------------------------------------------------------------------------
-- https://adventofcode.com/2023/day/22

p :: String -> [(P3, P3)]
p = fromJust . parse

s0 :: String
s0 =
  unpack
    [trimming|
1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9
|]

ss :: String
ss = unsafePerformIO $ loadDate 2023 22
{-# NOINLINE ss #-}
