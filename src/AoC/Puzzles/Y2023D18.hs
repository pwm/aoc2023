module AoC.Puzzles.Y2023D18 where

import AoC.Lib.Graph
import AoC.Lib.Grid
import AoC.Lib.Parser
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

{-
https://en.wikipedia.org/wiki/Pick%27s_theorem
-}

parse :: String -> Maybe [(Dir4, Int, String)]
parse = parseMaybe (lineP `sepEndBy` newline)

-- 76387
solveA :: [(Dir4, Int, String)] -> Int
solveA ls =
  let (d1, d2) = fromJust $ l2p $ take 2 $ map fstOf3 ls
   in Map.size $ fill (d1, d2) $ dig ls

solveB :: [(Dir4, Int, String)] -> ()
solveB _ = ()

type Grid = GridOf String

fill :: (Dir4, Dir4) -> Grid -> Grid
fill (d1, d2) g =
  Map.union g
    . Map.fromList
    . map ((,"X") . fst)
    . bfs (filter (`Map.notMember` g) . adj4)
    $ inside (0, 0) (d1, d2)

dig :: [(Dir4, Int, String)] -> Grid
dig = fst . foldl' digLine (mempty, (0, 0))
  where
    digLine :: (Grid, Pos) -> (Dir4, Int, String) -> (Grid, Pos)
    digLine (g, pos) (dir, stepCount, cell) =
      let steps = drop 1 $ take (stepCount + 1) $ iterate (`step4` dir) pos
       in (Map.fromList (map (,cell) steps) <> g, last steps)

inside :: Pos -> (Dir4, Dir4) -> Pos
inside (v, h) = \case
  (R, D) -> (v + 1, h + 1)
  (R, U) -> (v - 1, h + 1)
  (L, D) -> (v + 1, h - 1)
  (L, U) -> (v - 1, h - 1)
  (D, L) -> (v + 1, h - 1)
  (D, R) -> (v + 1, h + 1)
  (U, L) -> (v - 1, h - 1)
  (U, R) -> (v - 1, h + 1)
  _ -> error "not an angled line"

lineP :: Parser (Dir4, Int, String)
lineP = do
  d <- enumParser show parseDir4 <* char ' '
  n <- intP
  s <- strP "(#" *> count 6 alphaNumChar <* strP ")"
  pure (d, n, s)

parseDir4 :: String -> Maybe Dir4
parseDir4 = \case
  "U" -> Just U
  "R" -> Just R
  "D" -> Just D
  "L" -> Just L
  _ -> Nothing

-------------------------------------------------------------------------------
-- PP

ddg :: Grid -> IO ()
ddg = putStrLn . printGrid (\case "." -> "."; _ -> "X") . fillSpace

fillSpace :: Grid -> Grid
fillSpace g = g <> Map.fromList (map (,".") $ mkRect (bounds g))

---------------------------------------------------------------------------
-- https://adventofcode.com/2023/day/18

p :: String -> [(Dir4, Int, String)]
p = fromJust . parse

s0 :: String
s0 =
  unpack
    [trimming|
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
|]

ss :: String
ss = unsafePerformIO $ loadDate 2023 18
{-# NOINLINE ss #-}
