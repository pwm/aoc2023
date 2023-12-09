module AoC.Puzzles.Y2023D08 where

import AoC.Lib.Parser
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map

parse :: String -> Maybe ([Dir], Graph)
parse = parseMaybe taskP

solveA :: ([Dir], Graph) -> Int
solveA (dirs, graph) = length $ walk (dirs, graph) (== "ZZZ") "AAA"

solveB :: ([Dir], Graph) -> Int
solveB (dirs, graph) =
  let aStarts = Map.keys $ filterKey endA graph
   in lcms $ map (length . walk (dirs, graph) endZ) aStarts

endA, endZ :: String -> Bool
endA = (== 'A') . last
endZ = (== 'Z') . last

type Graph = Map String (String, String)

walk :: ([Dir], Graph) -> (String -> Bool) -> String -> [String]
walk (dirs, graph) found start = go start (cycle dirs)
  where
    go :: String -> [Dir] -> [String]
    go n _ | found n = []
    go n (d : ds) = n : go (lkp graph n d) ds
    go _ _ = []

lkp :: Graph -> String -> Dir -> String
lkp g n dir = case g !? n of
  Nothing -> error $ "no node " <> n
  Just (l, r) -> case dir of
    L -> l
    R -> r

data Dir = L | R
  deriving stock (Show, Eq, Ord, Bounded, Enum)

taskP :: Parser ([Dir], Graph)
taskP = do
  dirs <- some dirP <* newline <* newline
  nodes <- nodeP `sepEndBy` newline
  pure (dirs, Map.fromList nodes)

dirP :: Parser Dir
dirP = enumParser ppDir parseDir

nodeP :: Parser (String, (String, String))
nodeP = do
  n <- count 3 upperChar <* strP " ="
  l <- strP "(" *> count 3 upperChar
  r <- strP "," *> count 3 upperChar <* strP ")"
  pure (n, (l, r))

parseDir :: String -> Maybe Dir
parseDir = \case
  "L" -> Just L
  "R" -> Just R
  _ -> Nothing

ppDir :: Dir -> String
ppDir = \case
  L -> "L"
  R -> "R"
