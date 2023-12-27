module AoC.Puzzles.Y2023D25 where

import AoC.Lib.Parser
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Safe (atMay)
import System.Random qualified as Random
import System.Random.Stateful qualified as Random

parse :: String -> Maybe AdjList
parse = parseMaybe taskP

-- First ever solver that needs IO because of the RNG
solveA :: AdjList -> IO Int
solveA m = product . map Set.size . Map.keys <$> collapseTill 3 m

solveB :: AdjList -> IO ()
solveB _ = pure ()

type AdjList = Map (Set String) [Set String]

type Edge = (Set String, Set String)

-- https://en.wikipedia.org/wiki/Karger%27s_algorithm
collapseTill :: Int -> AdjList -> IO AdjList
collapseTill i m = do
  m' <- collapse m
  if Map.elems (Map.map length m') == [i, i]
    then pure m'
    else collapseTill i m

collapse :: AdjList -> IO AdjList
collapse m
  | Map.size m == 2 = pure m
  | otherwise = randomEdge m >>= collapse . contract m

contract :: AdjList -> Edge -> AdjList
contract m (from, to) = do
  let combinedN = Set.union from to
      combinedE = filter (/= to) (m ! from) <> filter (/= from) (m ! to)
      m' = Map.insert combinedN combinedE (Map.delete to (Map.delete from m))
   in Map.map (map (\n -> if n == from || n == to then combinedN else n)) m'

randomEdge :: AdjList -> IO Edge
randomEdge m = pickRandomEdge >>= maybe (randomEdge m) pure
  where
    pickRandomEdge :: IO (Maybe Edge)
    pickRandomEdge = do
      n1 <- rng (0, Map.size m - 1)
      let (from, tos) = Map.elemAt n1 m
      n2 <- rng (0, max 0 (length tos - 1))
      pure $ case atMay tos n2 of
        Just to | from /= to -> Just (from, to)
        _ -> Nothing

rng :: (Random.UniformRange a) => (a, a) -> IO a
rng (lo, hi) = Random.applyAtomicGen (Random.uniformR (lo, hi)) Random.globalStdGen

taskP :: Parser AdjList
taskP = adjList <$> (lineP `sepEndBy` newline)

lineP :: Parser (String, [String])
lineP = do
  from <- some lowerChar <* strP ":"
  tos <- some lowerChar `sepBy` char ' '
  pure (from, tos)

adjList :: [(String, [String])] -> AdjList
adjList = foldr addNode mempty
  where
    addNode :: (String, [String]) -> AdjList -> AdjList
    addNode (from, tos) m =
      Map.insertWith (<>) (Set.singleton from) (map Set.singleton tos) $
        foldr (\to -> Map.insertWith (<>) (Set.singleton to) [Set.singleton from]) m tos
