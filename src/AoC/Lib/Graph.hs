module AoC.Lib.Graph where

import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map
import Data.PQueue.Prio.Min (MinPQueue)
import Data.PQueue.Prio.Min qualified as MinPQueue
import Data.Sequence qualified as Seq
import Data.Set qualified as Set

topSort :: forall n. (Ord n) => (n -> [n]) -> [n] -> [n]
topSort nexts =
  map snd
    . sortOn (Down . fst)
    . flip evalState (mempty, 0)
    . fmap mconcat
    . traverse go
  where
    go :: n -> State (Set n, Int) [(Int, n)]
    go node = do
      seen <- use _1
      if Set.member node seen
        then pure []
        else do
          _1 %= Set.insert node
          res <- mconcat <$> traverse go (nexts node)
          n <- _2 += 1 >> use _2
          pure $ (n, node) : res

-- Explore all reachable nodes from a node
dfs :: (Ord n) => (n -> [n]) -> n -> [n]
dfs = dfsTo (const False)

-- Explore reachable nodes from a node to a dest node
dfsTo :: forall n. (Ord n) => (n -> Bool) -> (n -> [n]) -> n -> [n]
dfsTo found nexts from = go mempty [from]
  where
    go :: Set n -> [n] -> [n]
    go _ [] = []
    go seen (node : unseen)
      | found node = [node]
      | Set.member node seen = go seen unseen
      | otherwise = node : go (Set.insert node seen) (nexts node <> unseen)

-- Explore all reachable nodes from a list of nodes
-- Note: from a list of roots it will explore the entire graph
-- inefficient as the seen state is lost between roots
dfsAll :: (Ord n) => (n -> [n]) -> [n] -> [n]
dfsAll nexts = nubOrd . concatMap (dfs nexts)

-- All paths from node to a dest node
dfsPathsTo :: forall n. (Ord n) => (n -> Bool) -> (n -> [n]) -> n -> [[n]]
dfsPathsTo found nexts = map reverse . go []
  where
    go :: [n] -> n -> [[n]]
    go path node
      | found node = [node : path]
      | node `elem` path = []
      | otherwise = nexts node >>= go (node : path)

-- All paths from node to all sink nodes
-- dfs nexts n == nubOrd (concat (dfsPaths nexts n))
dfsPaths :: (Ord n) => (n -> [n]) -> n -> [[n]]
dfsPaths nexts = dfsPathsTo (null . nexts) nexts -- does not handle cycles (ie. from == node)

-- Literally select the shortest path to a dest node
dfsSPTo :: (Ord n) => (n -> Bool) -> (n -> [n]) -> n -> [n]
dfsSPTo found nexts = minimumBy (comparing length) . dfsPathsTo found nexts

--

bfs :: (Ord n) => (n -> [n]) -> n -> [(n, Int)]
bfs = bfsTill (const False)

bfsTill :: forall n. (Ord n) => (n -> Bool) -> (n -> [n]) -> n -> [(n, Int)]
bfsTill found next from = go mempty (Seq.fromList [(from, 0)])
  where
    go :: Set n -> Seq.Seq (n, Int) -> [(n, Int)]
    go _ Seq.Empty = []
    go seen ((node, d) :<| unseen)
      | found node = [(node, d)]
      | Set.member node seen = go seen unseen
      | otherwise = (node, d) : go (Set.insert node seen) (unseen <> Seq.fromList ((,d + 1) <$> next node))

-- Shortest path from root to dest
bfsSPTo :: (Ord n) => (n -> [n]) -> n -> n -> [(n, Int)]
bfsSPTo nexts dest = buildSP dest . bfsWithParents nexts (== dest)

-- Shortest-path tree (aka. BFS tree which, in unweighted graphs, coincide with the SPT)
bfsSPT :: (Ord n) => (n -> [n]) -> n -> Tree (n, Int)
bfsSPT nexts = buildSPT . bfsWithParents nexts (const False)

-- Keeps track of parents. BFS as a special case of Dijkstra's with weights of 1
bfsWithParents :: (Ord n) => (n -> [n]) -> (n -> Bool) -> n -> [(n, (Int, Maybe n))]
bfsWithParents nexts = dijkstraWithParents (map (,1) . nexts)

--

dijkstra :: (Ord n) => (n -> [(n, Int)]) -> n -> [(n, Int)]
dijkstra = dijkstraTill (const False)

dijkstraTill :: (Ord n) => (n -> Bool) -> (n -> [(n, Int)]) -> n -> [(n, Int)]
dijkstraTill found nexts = map (second fst) . dijkstraWithParents nexts found

-- Shortest path with dists from root to dest
dijkstraSPTo :: (Ord n) => (n -> [(n, Int)]) -> n -> n -> [(n, Int)]
dijkstraSPTo nexts dest = buildSP dest . dijkstraWithParents nexts (== dest)

-- Shortest-path tree with dists
dijkstraSPT :: (Ord n) => (n -> [(n, Int)]) -> n -> Tree (n, Int)
dijkstraSPT nexts = buildSPT . dijkstraWithParents nexts (const False)

-- Dijkstra's as a special case of A* using 0 for heuristic
dijkstraWithParents :: (Ord n) => (n -> [(n, Int)]) -> (n -> Bool) -> n -> [(n, (Int, Maybe n))]
dijkstraWithParents nexts = astarWithParents (map (second (,0)) . nexts)

--

astar :: (Ord n) => (n -> [(n, (Int, Int))]) -> n -> [(n, Int)]
astar = astarTill (const False)

astarTill :: (Ord n) => (n -> Bool) -> (n -> [(n, (Int, Int))]) -> n -> [(n, Int)]
astarTill found nexts = map (second fst) . astarWithParents nexts found

astarSPTo :: (Ord n) => (n -> [(n, (Int, Int))]) -> n -> n -> [(n, Int)]
astarSPTo nexts dest = buildSP dest . astarWithParents nexts (== dest)

astarSPT :: (Ord n) => (n -> [(n, (Int, Int))]) -> n -> Tree (n, Int)
astarSPT nexts = buildSPT . astarWithParents nexts (const False)

-- The most generic algo here, uses heuristic and keeps track of parents
astarWithParents ::
  forall n.
  (Ord n) =>
  (n -> [(n, (Int, Int))]) ->
  (n -> Bool) ->
  n ->
  [(n, (Int, Maybe n))]
astarWithParents nexts found from = go mempty (MinPQueue.singleton 0 (from, Nothing))
  where
    go :: Set n -> MinPQueue Int (n, Maybe n) -> [(n, (Int, Maybe n))]
    go seen unseen = case MinPQueue.minViewWithKey unseen of
      Nothing -> []
      Just ((cost, (node, parent)), unseen')
        | found node -> [(node, (cost, parent))]
        | Set.member node seen -> go seen unseen'
        | otherwise ->
            let addNode :: MinPQueue Int (n, Maybe n) -> (n, (Int, Int)) -> MinPQueue Int (n, Maybe n)
                addNode mpq (n, (stepCost, h)) = MinPQueue.insert (cost + stepCost + h) (n, Just node) mpq
             in (node, (cost, parent)) : go (Set.insert node seen) (foldl' addNode unseen' (nexts node))

-- Builds shortest path from root (identified by Nothing as its parent) to dest
buildSP :: forall n. (Ord n) => n -> [(n, (Int, Maybe n))] -> [(n, Int)]
buildSP dest (Map.fromList -> m) = reverse $ go [] dest
  where
    go :: [(n, Int)] -> n -> [(n, Int)]
    go path node = case m !? node of
      Nothing -> []
      Just (cost, mp) -> case mp of
        Nothing -> (node, cost) : path
        Just parent -> (node, cost) : go path parent

-- Builds shortest path tree from root (identified by Nothing as its parent)
buildSPT :: forall n. (Ord n) => [(n, (Int, Maybe n))] -> Tree (n, Int)
buildSPT (Map.fromList -> m) =
  let (root, adjList) = Map.foldrWithKey mkAdjList (error "Root not found", mempty) m
   in unfoldTree (\(n, d) -> ((n, d), fromMaybe [] (adjList !? n))) (root, 0)
  where
    mkAdjList :: n -> (Int, Maybe n) -> (n, Map n [(n, Int)]) -> (n, Map n [(n, Int)])
    mkAdjList n (_, Nothing) (_, adjList) = (n, adjList) -- root
    mkAdjList n (d, Just p) (r, adjList) = (r, Map.insertWith (<>) p [(n, d)] adjList)

-- All paths of the SPT
sptPaths :: Tree n -> [[n]]
sptPaths (Node n []) = [[n]]
sptPaths (Node n ts) = map (n :) (concatMap sptPaths ts)
