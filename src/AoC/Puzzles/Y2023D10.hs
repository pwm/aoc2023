module AoC.Puzzles.Y2023D10 where

import AoC.Lib.Graph
import AoC.Lib.Grid
import AoC.Lib.Parser
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

parse :: String -> Maybe Grid
parse = parseGrid parseCell

solveA :: Grid -> Int
solveA g = (`div` 2) . length . fstOf3 $ walk False (fst $ startDirs g) g

solveB :: Grid -> Int
solveB g =
  let (dir1, dir2) = startDirs g
      (loop, _, _) = walk False dir1 g -- walk once to find the loop
      g' = Map.restrictKeys g loop `Map.union` Map.map (const Dot) g -- remove unused pipes
      -- walk the loop again both directions and check clockwise/anticlockwise
      (_, clock1, anticlock1) = walk True dir1 g'
      (_, anticlock2, clock2) = walk True dir2 g'
   in -- the minimum of the two should be the sum of inner spaces
      Set.size (clock1 `Set.union` clock2) `min` Set.size (anticlock1 `Set.union` anticlock2)

walk :: Bool -> Dir4 -> Grid -> (Set Pos, Set Pos, Set Pos)
walk checkNeighbours sDir g = execState (go (start g) sDir) (mempty, mempty, mempty)
  where
    go :: Pos -> Dir4 -> State (Set Pos, Set Pos, Set Pos) ()
    go p d = do
      (loop, cps, acps) <- get
      if not (null loop) && g ! p == Start
        then pure ()
        else do
          let (p', d') = stepDir p (g ! p) d
          _1 %= Set.insert p'
          -- check clockwise and anticlockwise dots and floodfill them
          when checkNeighbours $ do
            let cp = step4 p' (clock4 d')
            when (g !? cp == Just Dot) $ do
              let ps = fst <$> bfs (filter (\p1 -> g !? p1 == Just Dot && Set.notMember p1 cps) . neighbours4 g) cp
              _2 %= Set.union (Set.fromList ps)
            let acp = step4 p' (anticlock4 d')
            when (g !? acp == Just Dot) $ do
              let ps = fst <$> bfs (filter (\p1 -> g !? p1 == Just Dot && Set.notMember p1 acps) . neighbours4 g) acp
              _3 %= Set.union (Set.fromList ps)
          go p' d'

stepDir :: Pos -> Cell -> Dir4 -> (Pos, Dir4)
stepDir p c d =
  let d' = case c of
        UR -> case d of D -> R; _ -> U
        UL -> case d of D -> L; _ -> U
        DL -> case d of U -> L; _ -> D
        DR -> case d of U -> R; _ -> D
        _ -> d
   in (step4 p d', d')

startDirs :: Grid -> (Dir4, Dir4)
startDirs g | p <- start g =
  case idStart p of UR -> (U, R); UL -> (U, L); DL -> (D, L); _ -> (D, R)
  where
    idStart :: Pos -> Cell
    idStart p
      | g !? step4 p U `elem` (Just <$> [Ver, DR, DL]) && g !? step4 p R `elem` (Just <$> [Hor, UL, DL]) = UR
      | g !? step4 p U `elem` (Just <$> [Ver, DR, DL]) && g !? step4 p L `elem` (Just <$> [Hor, DR, UR]) = UL
      | g !? step4 p D `elem` (Just <$> [Ver, UR, UL]) && g !? step4 p L `elem` (Just <$> [Hor, UR, DR]) = DL
      | otherwise = DR

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
