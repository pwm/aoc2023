module AoC.Puzzles.Y2023D06 where

import AoC.Lib.Parser
import AoC.Lib.Prelude

parse :: String -> Maybe ([Int], [Int])
parse = parseMaybe taskP

solveA :: ([Int], [Int]) -> Int
solveA = product . map records . uncurry zip

solveB :: ([Int], [Int]) -> Int
solveB (ts, rs) = records (read (concatMap show ts), read (concatMap show rs))

-- dist = (limit - speed) * speed
-- -(speed * speed) + limit * speed - dist = 0
records :: (Int, Int) -> Int
records (timeLimit, toBeat) =
  let (l, h) = qSolver @Double (-1) (fromIntegral timeLimit) (fromIntegral (-toBeat))
      li = let cl = ceiling l in if isWhole l then cl + 1 else cl
      hi = let fh = floor h in if isWhole h then fh - 1 else fh
   in hi - li + 1

qSolver :: (Floating b) => b -> b -> b -> (b, b)
qSolver a b c =
  let f op = (-b `op` sqrt (b * b - 4 * a * c)) / (2 * a)
   in (f (-), f (+))

isWhole :: (RealFrac w) => w -> Bool
isWhole x = x == fromIntegral (floor @_ @Int x)

taskP :: Parser ([Int], [Int])
taskP = do
  ts <- strP "Time:" *> some intP <* newline
  ds <- strP "Distance:" *> some intP <* optional newline
  pure (ts, ds)
