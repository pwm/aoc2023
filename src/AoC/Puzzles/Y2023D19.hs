{-# OPTIONS_GHC -Wno-partial-fields #-}

module AoC.Puzzles.Y2023D19 where

import AoC.Lib.Interval (IV (..))
import AoC.Lib.Parser
import AoC.Lib.Prelude hiding (Tree (..), id)
import Data.Map.Strict qualified as Map

parse :: String -> Maybe (FSM, [Part])
parse = parseMaybe taskP

solveA :: (FSM, [Part]) -> Int
solveA (fsm, parts) =
  let goods = filter (eval fsm) parts
   in sum $ map (\p -> p.x + p.m + p.a + p.s) goods

solveB :: (FSM, [Part]) -> Int
solveB (fsm, _) =
  let ivm0 = Map.fromList $ map (,IV 1 4001) [X, M, A, S]
   in sum $ map (product . map (\iv -> iv.hi - iv.lo) . Map.elems) $ splitAll fsm ivm0

type FSM = Map String Node

data Node = Node {id :: String, nexts :: [Rule]}
  deriving stock (Show, Eq, Ord, Generic)

data Rule
  = Guard {to :: String, cond :: Cond}
  | Pass {to :: String}
  deriving stock (Show, Eq, Ord, Generic)

data Cond = Cond {var :: Var, comp :: Comp, val :: Int}
  deriving stock (Show, Eq, Ord, Generic)

data Var = X | M | A | S
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

data Comp = Lt | Gt
  deriving stock (Show, Eq, Ord, Generic)

data Part = Part {x :: Int, m :: Int, a :: Int, s :: Int}
  deriving stock (Show, Eq, Ord, Generic)

---------------------------------------------------------------------------
-- Part A

eval :: FSM -> Part -> Bool
eval fsm part = go "in"
  where
    go :: String -> Bool
    go nid = case transition part (fsm ! nid).nexts of
      "A" -> True
      "R" -> False
      next -> go next

transition :: Part -> [Rule] -> String
transition part = \case
  [] -> "R"
  (rule : rules)
    | Just to <- applyRule part rule -> to
    | otherwise -> transition part rules

applyRule :: Part -> Rule -> Maybe String
applyRule part = \case
  Pass to -> Just to
  Guard to (Cond var (compFn -> op) n)
    | valOf part var `op` n -> Just to
    | otherwise -> Nothing

compFn :: (Ord a) => Comp -> (a -> a -> Bool)
compFn = \case Lt -> (<); Gt -> (>)

valOf :: Part -> Var -> Int
valOf part = \case X -> part.x; M -> part.m; A -> part.a; S -> part.s

---------------------------------------------------------------------------
-- Part B

type IVMap = Map Var (IV Int)

splitAll :: FSM -> IVMap -> [IVMap]
splitAll fsm ivm0 = go (ivm0, "in")
  where
    go :: (IVMap, String) -> [IVMap]
    go (ivm, nid)
      | nid == "A" = [ivm]
      | otherwise = concatMap go $ splitOnRules ivm (fsm ! nid).nexts

splitOnRules :: IVMap -> [Rule] -> [(IVMap, String)]
splitOnRules _ [] = []
splitOnRules ivm (rule : rules) =
  let (res, ivm') = splitOnRule ivm rule
   in res : splitOnRules ivm' rules

splitOnRule :: IVMap -> Rule -> ((IVMap, String), IVMap)
splitOnRule ivm = \case
  Pass to -> ((ivm, to), mempty)
  Guard to (Cond var comp n) ->
    let iv = ivm ! var
        (cutIv, remIv) = case comp of
          Lt -> (iv {hi = n}, iv {lo = n})
          Gt -> (iv {lo = n + 1}, iv {hi = n + 1})
     in ((Map.insert var cutIv ivm, to), Map.insert var remIv ivm)

---------------------------------------------------------------------------
-- Parser

taskP :: Parser (FSM, [Part])
taskP = do
  nodes <- nodeP `sepEndBy` newline <* newline
  parts <- partP `sepEndBy` newline
  let fsm = foldr (\node -> Map.insert node.id node) sinks nodes
  pure (fsm, parts)
  where
    sinks :: FSM
    sinks = Map.fromList [("A", Node "A" []), ("R", Node "R" [])]

nodeP :: Parser Node
nodeP = do
  id <- lowerChar `someTill` char '{'
  rules <- ruleP `sepBy` char ',' <* char '}'
  pure $ Node id rules

ruleP :: Parser Rule
ruleP = do
  mcond <- optional (try (condP <* char ':'))
  to <- strP "A" <|> strP "R" <|> some lowerChar
  pure $ maybe (Pass to) (Guard to) mcond

condP :: Parser Cond
condP = liftA3 Cond varP ((strP "<" $> Lt) <|> (strP ">" $> Gt)) intP

partP :: Parser Part
partP = do
  x <- strP "{x=" *> intP
  m <- strP ",m=" *> intP
  a <- strP ",a=" *> intP
  s <- strP ",s=" *> intP <* strP "}"
  pure $ Part x m a s

varP :: Parser Var
varP = enumParser (fmap toLower . show) $ \case
  "x" -> Just X
  "m" -> Just M
  "a" -> Just A
  "s" -> Just S
  _ -> Nothing
