module AoC.Puzzles.Y2023D23 where

import AoC.Lib.Dot
import AoC.Lib.Graph
import AoC.Lib.Grid
import AoC.Lib.Parser
import AoC.Lib.Prelude
import Control.Monad.Logic
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

parse :: String -> Maybe Grid
parse = fmap (addJunctions . Map.filter (/= Bush)) . parseGrid parseCell

solveA :: Grid -> Int
solveA = subtract 1 . length . solA
  where
    solA :: Grid -> [Pos]
    solA g =
      let (start, end) = bounds g
       in dfsLPTo (== end) nexts start
      where
        nexts :: Pos -> [Pos]
        nexts p =
          concat
            [ let p' = p <+> stepU in p' <$ filter (/= DownSlope) (maybeToList (g !? p')),
              let p' = p <+> stepL in p' <$ filter (/= RightSlope) (maybeToList (g !? p')),
              let p' = p <+> stepR in maybe [] (const [p']) (g !? p'),
              let p' = p <+> stepD in maybe [] (const [p']) (g !? p')
            ]

solveB :: Grid -> Int
solveB = findLongestWalk . buildWeightedGraph

data Cell = Dot | Bush | RightSlope | DownSlope | Junction
  deriving stock (Show, Eq, Ord, Generic)

type Grid = GridOf Cell

type Graph = Map Pos [(Pos, Int)]

data Candidate = Candidate
  {graph :: Graph, pos :: Pos, total :: Int}
  deriving stock (Show, Eq, Ord, Generic)

findLongestWalk :: Graph -> Int
findLongestWalk graph =
  let e0 = fst $ Map.findMax graph
      c0 = Candidate graph (fst $ Map.findMin graph) 0
   in runReader (execStateT (observeAllT (go c0)) 0) e0
  where
    go :: Candidate -> LogicT (StateT Int (Reader Pos)) ()
    go c = do
      best <- get
      (pos', cost) <- choose $ sortOn (Down . snd) $ fromMaybe [] $ c.graph !? c.pos
      let total' = c.total + cost
          g' = rmNode c.pos c.graph
      end <- ask
      if
          | pos' == end && total' > best -> put total'
          -- prune when done without a best or when the expected
          -- theoretical max is less than the current best
          | pos' == end || total' + maxValue g' <= best -> empty
          | otherwise -> go $ Candidate g' pos' total'
    maxValue :: Graph -> Int
    maxValue = (`div` 2) . Map.foldr (\es acc -> acc + sum (map snd es)) 0
    rmNode :: Pos -> Graph -> Graph
    rmNode pos = Map.map (filter ((/= pos) . fst)) . Map.delete pos

data Env = Env {grid :: Grid, end :: Pos}
  deriving stock (Show, Eq, Ord, Generic)

data Path = Path
  { pos :: Pos,
    cell :: Cell,
    from :: Pos,
    steps :: Int,
    seen :: Set Pos
  }
  deriving stock (Show, Eq, Ord, Generic)

-- Transform the maze into a weighted graph by connecting junctions
buildWeightedGraph :: Grid -> Graph
buildWeightedGraph grid =
  let (start, end) = bounds grid
      env = Env grid end
      path = Path start (grid ! start) start (-1) mempty
   in runReader (execStateT (observeAllT (go path)) (Map.singleton start [])) env
  where
    go :: Path -> LogicT (StateT Graph (Reader Env)) ()
    go path = do
      env <- ask
      graph <- get
      guard $ -- only add edges once
        ((path.pos `notElem`) . map fst) (graph ! path.from)
      -- add a weighted edge (by connecting 2 distinct, neighbouring junctions)
      when (path.cell == Junction && path.from /= path.pos) $ do
        modify $ Map.insertWith (<>) path.from [(path.pos, path.steps + 1)]
        modify $ Map.insertWith (<>) path.pos [(path.from, path.steps + 1)]
      (pos', cell') <-
        choose
          . filter ((`Set.notMember` path.seen) . fst)
          $ lookupKNs4 path.pos env.grid
      go $
        path
          { pos = pos',
            cell = cell',
            from = if path.cell == Junction then path.pos else path.from,
            steps = if path.cell == Junction then 0 else path.steps + 1,
            seen = Set.insert path.pos path.seen
          }

-- mark T and X junctions (nb.: the end is also treated as a junction for ease)
addJunctions :: Grid -> Grid
addJunctions grid =
  let end = snd $ bounds grid
      addJunction :: Pos -> Cell -> Cell
      addJunction p c
        | ns <- lookupKNs4 p grid, length ns >= 3 = Junction
        | p == end = Junction
        | otherwise = c
   in Map.mapWithKey addJunction grid

parseCell :: Char -> Maybe Cell
parseCell = \case
  '.' -> Just Dot
  '>' -> Just RightSlope
  'v' -> Just DownSlope
  '#' -> Just Bush
  _ -> Nothing

---------------------------------------------------------------------------
-- Dot

ddg :: String -> Graph -> IO ()
ddg s g =
  let lblr :: (Pos, Pos) -> Int
      lblr (from, to) = fromMaybe 0 $ lookup to (fromMaybe [] $ g !? from)
   in ddd s . toDotGraphLbl @_ @Text Nothing (Just lblr) Undirected . toDot $ g

toDot :: Graph -> DotG Pos
toDot g =
  let (vs, es) = unzip $ map (\(k, vs') -> (k, map ((k,) . fst) vs')) $ Map.toList g
   in DotG vs (concat es)
