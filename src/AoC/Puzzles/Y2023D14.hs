module AoC.Puzzles.Y2023D14 where

import AoC.Lib.Grid
import AoC.Lib.Parser
import AoC.Lib.Prelude hiding (load)
import Data.Hashable
import Data.Map.Strict qualified as Map

parse :: String -> Maybe Grid
parse = parseGrid parseCell

solveA :: Grid -> Int
solveA = load . roll U

solveB :: Grid -> Int
solveB = spinBillion

spinBillion :: Grid -> Int
spinBillion = flip evalState (mempty, 1) . go
  where
    go :: Grid -> State (Map Int Int, Int) Int
    go g = do
      let g' = spin g
      (m, c) <- get
      if c >= 1_000_000_000
        then pure $ load g'
        else do
          let c' =
                if Map.notMember (hash g') m
                  then c + 1
                  else
                    let cLast = m ! hash g'
                     in 1_000_000_000 - (1_000_000_000 - cLast - 1) `mod` (c - cLast)
          put (Map.insert (hash g') c m, c')
          go g'

load :: Grid -> Int
load g =
  let (vMax, _) = bimap (+ 1) (+ 1) $ fst $ Map.findMax g
   in sum $ map (\(v, _) -> vMax - v) $ rounds g

spin :: Grid -> Grid
spin = roll R . roll D . roll L . roll U

roll :: Dir4 -> Grid -> Grid
roll dir = fixpoint (roll1 dir)

roll1 :: Dir4 -> Grid -> Grid
roll1 dir g0 = foldl' go g0 (rounds g0)
  where
    go :: Grid -> Pos -> Grid
    go g p =
      let p' = step4 p dir
       in case g !? p' of
            Just Dot -> Map.insert p' Round (Map.insert p Dot g)
            _ -> g

rounds :: Grid -> [Pos]
rounds = Map.keys . Map.filter (== Round)

type Grid = GridOf Cell

data Cell = Dot | Square | Round
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance Hashable Cell

parseCell :: Char -> Maybe Cell
parseCell = \case
  '.' -> Just Dot
  '#' -> Just Square
  'O' -> Just Round
  _ -> Nothing
