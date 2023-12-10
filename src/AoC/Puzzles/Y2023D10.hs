module AoC.Puzzles.Y2023D10 where

import AoC.Lib.Graph
import AoC.Lib.Grid
import AoC.Lib.Parser
import AoC.Lib.Prelude
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

-- import Data.Functor.Identity

parse :: String -> Maybe Grid
parse = parseGrid parseCell

-- 6838
solveA :: Grid -> Int
solveA = const 0 -- (`div` 2) . length . walk

-- 451
solveB :: Grid -> ()
solveB _ = ()

-- putStrLn $ ppg $ f4 ss
f4 :: String -> Grid
f4 s =
  let g = pr s
      (ps, _) = walk clockwise g D
      g' = f1 ps g
      (_, rs1) = walk clockwise g' D
      (_, rs2) = walk anticlockwise g' R
   in f2 (Set.toList $ Set.union rs1 rs2) g'

-- let g = pr s3; (ps, rs) = walk g in putStrLn (ppg $ f2 (Set.toList rs) $ f1 ps g)
f2 :: [Pos] -> Grid -> Grid
f2 ps g = g'
  where
    gWith = Map.restrictKeys g (Set.fromList ps)
    g' = Map.merge Map.preserveMissing Map.preserveMissing (Map.zipWithMatched (\_ _ _ -> Start)) g gWith

-- let g = pr s3; (ps, rs) = walk g in putStrLn (ppg $ f1 ps g)
f1 :: [Pos] -> Grid -> Grid
f1 ps g = g'
  where
    gWith = Map.restrictKeys g (Set.fromList ps)
    gWithout = Map.map (const Dot) $ Map.withoutKeys g (Set.fromList ps)
    g' = Map.merge Map.preserveMissing Map.preserveMissing (Map.zipWithMatched (\_ _ _ -> Dot)) gWith gWithout

-- let g = pr s3 in walk g
walk :: (Pos -> Dir4 -> Pos) -> Grid -> Dir4 -> ([Pos], Set Pos)
walk turn g0 sDir = snd $ execState (go (start g0, sDir)) (g0, ([], mempty))
  where
    go :: (Pos, Dir4) -> State (Grid, ([Pos], Set Pos)) ()
    go (p, d) = do
      (g, (ps, rs)) <- get
      if not (null ps) && g ! p == Start
        then pure ()
        else do
          let (p', d') = step1 g (p, d)
          (_2 . _1) %= (p' :)
          let cw1 = turn p' d'
          when (g !? cw1 == Just Dot) $ do
            let xs = map fst $ bfs (filter (\p1 -> g !? p1 == Just Dot && Set.notMember p1 rs) . neighbours4 g) cw1
            (_2 . _2) %= Set.union (Set.fromList xs)
          go (p', d')

step1 :: Grid -> (Pos, Dir4) -> (Pos, Dir4)
step1 g (p, d) = case g ! p of
  Start -> (step4 p d, d)
  Hor -> (step4 p d, d)
  Ver -> (step4 p d, d)
  UR -> let d' = case d of D -> R; _ -> U in (step4 p d', d')
  UL -> let d' = case d of D -> L; _ -> U in (step4 p d', d')
  DL -> let d' = case d of U -> L; _ -> D in (step4 p d', d')
  DR -> let d' = case d of U -> R; _ -> D in (step4 p d', d')
  Dot -> error "nooooo"

clockwise, anticlockwise :: Pos -> Dir4 -> Pos
clockwise p = step4 p . clock4
anticlockwise p = step4 p . anticlock4

start :: Grid -> Pos
start = head . Map.keys . Map.filter (== Start)

type Grid = GridOf Cell

data Cell = Hor | Ver | UR | UL | DL | DR | Dot | Start
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

parseCell :: Char -> Maybe Cell
parseCell = \case
  '-' -> Just Hor
  '|' -> Just Ver
  'L' -> Just UR
  'J' -> Just UL
  '7' -> Just DL
  'F' -> Just DR
  '.' -> Just Dot
  'S' -> Just Start
  _ -> Nothing

ppCell :: Cell -> String
ppCell = \case
  Hor -> "─"
  Ver -> "│"
  UR -> "└"
  UL -> "┘"
  DL -> "┐"
  DR -> "┌"
  Dot -> "."
  Start -> "S"

ppg :: GridOf Cell -> String
ppg = printGrid ppCell

---------------------------------------------------------------------------
-- Test data

pr :: String -> Grid
pr = fromJust . parse

s0, s1, s2, s3, s4 :: String
s0 =
  unpack
    [trimming|
-L|F7
7S-7|
L|7||
-L-J|
L|-JF
|]
s1 =
  unpack
    [trimming|
7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ
|]
s2 =
  unpack
    [trimming|
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
|]
s3 =
  unpack
    [trimming|
.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
|]
s4 =
  unpack
    [trimming|
FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
|]

-- load actual input from the downloaded file
ss :: String
ss = unsafePerformIO $ loadDate 2023 10
{-# NOINLINE ss #-}
