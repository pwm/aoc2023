module AoC.Puzzles.Y2023D16 where

import AoC.Lib.Graph
import AoC.Lib.Grid
import AoC.Lib.Parser
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

parse :: String -> Maybe Grid
parse = parseGrid parseCell

solveA :: Grid -> Int
solveA g = run g ((0, 0), R)

solveB :: Grid -> Int
solveB g = maximum $ parMap rpar (run g) (edges g)

type Grid = GridOf Cell

data Cell = Dot | HSplit | VSplit | Slash | BSlash
  deriving stock (Show, Eq, Ord, Bounded, Enum)

edges :: Grid -> [(Pos, Dir4)]
edges g =
  let ((vl, hl), (vh, hh)) = bounds g
      du = concat [[((vl, h), D), ((vh, h), U)] | h <- [hl .. hh]]
      lr = concat [[((v, hl), R), ((v, hh), L)] | v <- [vl .. vh]]
   in du <> lr

run :: Grid -> (Pos, Dir4) -> Int
run g = Set.size . Set.fromList . map (fst . fst) . bfs (nexts g)

nexts :: Grid -> (Pos, Dir4) -> [(Pos, Dir4)]
nexts g (p, d) =
  let ds = case g !? p of
        Just Dot -> [d]
        Just HSplit -> if d `elem` [U, D] then [L, R] else [d]
        Just VSplit -> if d `elem` [L, R] then [U, D] else [d]
        Just Slash -> case d of U -> [R]; R -> [U]; D -> [L]; L -> [D]
        Just BSlash -> case d of U -> [L]; L -> [U]; D -> [R]; R -> [D]
        _ -> []
   in [(p', d') | d' <- ds, let p' = step4 p d', Map.member p' g]

parseCell :: Char -> Maybe Cell
parseCell = \case
  '.' -> Just Dot
  '-' -> Just HSplit
  '|' -> Just VSplit
  '/' -> Just Slash
  '\\' -> Just BSlash
  _ -> Nothing
