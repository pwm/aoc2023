module AoC.Puzzles.Y2023D16 where

import AoC.Lib.Grid
import AoC.Lib.Parser
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

parse :: String -> Maybe Grid
parse = parseGrid parseCell

solveA :: Grid -> Int
solveA = run ((0, 0), R)

solveB :: Grid -> Int
solveB g = maximum $ map (`run` g) (frame g)

type Grid = GridOf Cell

data Cell = Dot | HSplit | VSplit | Slash | BSlash
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

frame :: Grid -> [(Pos, Dir4)]
frame g =
  let ((vl, hl), (vh, hh)) = bounds g
      du = concat [[((vl, h), D), ((vh, h), U)] | h <- [hl .. hh]]
      lr = concat [[((v, hl), R), ((v, hh), L)] | v <- [vl .. vh]]
   in du <> lr

run :: (Pos, Dir4) -> Grid -> Int
run pd g = Set.size $ Set.map fst $ execState (go (Set.fromList [pd])) mempty
  where
    go :: Set (Pos, Dir4) -> State (Set (Pos, Dir4)) ()
    go moves = do
      s <- get
      if Set.null moves
        then pure ()
        else do
          modify $ Set.union moves
          let newMoves = Set.foldl' (\acc (p, d) -> Set.union (steps1 g p d) acc) mempty moves
          go $ Set.filter (\(p, d) -> Map.member p g && Set.notMember (p, d) s) newMoves

steps1 :: Grid -> Pos -> Dir4 -> Set (Pos, Dir4)
steps1 g p d =
  let ds = case g !? p of
        Just Dot -> [d]
        Just HSplit -> if d `elem` [U, D] then [L, R] else [d]
        Just VSplit -> if d `elem` [L, R] then [U, D] else [d]
        Just Slash -> case d of U -> [R]; R -> [U]; D -> [L]; L -> [D]
        Just BSlash -> case d of U -> [L]; L -> [U]; D -> [R]; R -> [D]
        _ -> []
   in Set.fromList $ map (\d' -> (step4 p d', d')) ds

parseCell :: Char -> Maybe Cell
parseCell = \case
  '.' -> Just Dot
  '-' -> Just HSplit
  '|' -> Just VSplit
  '/' -> Just Slash
  '\\' -> Just BSlash
  _ -> Nothing
