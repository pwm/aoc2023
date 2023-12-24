module AoC.Puzzles.Y2023D24 where

import AoC.Lib.Parser hiding (tab)
import AoC.Lib.Prelude
import Numeric.LinearAlgebra

parse :: String -> Maybe [(P3, P3)]
parse = parseMaybe (lineP `sepEndBy` newline)

-- 15318
solveA :: [(P3, P3)] -> Int
solveA =
  length
    . filter (\(tab, xy) -> future tab && inTarget xy)
    . mapMaybe solve2D
    . pairUp

solveB :: [(P3, P3)] -> ()
solveB _ = ()

{-
The 2 lines:
Line a: (ax, ay) = (ax0 + ta * vax, ay0 + ta * vay)
Line b: (bx, by) = (bx0 + tb * vbx, by0 + tb * vby)

At the intersection point the coordinates must be equal, so the following hold:
ta * vax - tb * vbx = bx0 - ax0
ta * vay - tb * vby = by0 - ay0

We solve for (ta, tb) and substitute either of them back into their respective
line equation to get the intersection point.
-}
solve2D :: ((P3, P3), (P3, P3)) -> Maybe ((Double, Double), (Double, Double))
solve2D (((ax0, ay0, _), (vax, vay, _)), ((bx0, by0, _), (vbx, vby, _))) =
  let sol =
        linearSolveLS
          ((2 >< 2) [vax, -vbx, vay, -vby])
          ((2 >< 1) [bx0 - ax0, by0 - ay0])
   in case concat (toLists sol) of
        [ta, tb] -> Just ((ta, tb), (ax0 + ta * vax, ay0 + ta * vay))
        _ -> Nothing

inTarget, future :: (Double, Double) -> Bool
inTarget (x, y) = 2e14 <= x && x <= 4e14 && 2e14 <= y && y <= 4e14
future (ta, tb) = ta >= 0 && tb >= 0

pairUp :: [(P3, P3)] -> [((P3, P3), (P3, P3))]
pairUp = fromJust . traverse l2p . pick 2

type P3 = (Double, Double, Double)

lineP :: Parser (P3, P3)
lineP = liftA2 (,) (p3P <* strP "@") v3P

p3P, v3P :: Parser P3
p3P = liftA3 (,,) (doubleP <* strP ",") (doubleP <* strP ",") doubleP
v3P = liftA3 (,,) (signedDoubleP <* strP ",") (signedDoubleP <* strP ",") signedDoubleP

doubleP, signedDoubleP :: Parser Double
doubleP = fromIntegral <$> intP
signedDoubleP = fromIntegral <$> signedIntP

---------------------------------------------------------------------------
-- https://adventofcode.com/2023/day/24

p :: String -> [(P3, P3)]
p = fromJust . parse

s00 :: (P3, P3)
s00 = ((24.0, 13.0, 10.0), (-3.0, 1.0, 2.0))

s0 :: String
s0 =
  unpack
    [trimming|
19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
|]

ss :: String
ss = unsafePerformIO $ loadDate 2023 24
{-# NOINLINE ss #-}
