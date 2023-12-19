{-# OPTIONS_GHC -Wno-partial-fields #-}

module AoC.Puzzles.Y2023D19 where

import AoC.Lib.Dot
import AoC.Lib.Parser
import AoC.Lib.Prelude hiding (Tree (..), id)
import Data.Map.Strict qualified as Map

parse :: String -> Maybe (FSM, [Part])
parse = parseMaybe taskP

-- 350678
solveA :: (FSM, [Part]) -> Int
solveA (fsm, parts) = sum $ map (\p -> p.x + p.m + p.a + p.s) $ filter (eval fsm) parts

solveB :: (FSM, [Part]) -> ()
solveB _ = ()

eval :: FSM -> Part -> Bool
eval fsm part = go "in"
  where
    go :: String -> Bool
    go cur = case fsm !? cur of
      Nothing -> False
      Just node -> case transition part node.nexts of
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

type FSM = Map String Node

data Node = Node {id :: String, nexts :: [Rule], prevs :: [Rule]}
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
-- Parser

taskP :: Parser (FSM, [Part])
taskP = do
  nodes <- nodeP `sepEndBy` newline <* newline
  parts <- partP `sepEndBy` newline
  pure (addPrevs $ foldr (\node -> Map.insert node.id node) terminals nodes, parts)

terminals :: FSM
terminals = Map.fromList [("A", Node "A" [] []), ("R", Node "R" [] [])]

addPrevs :: FSM -> FSM
addPrevs fsm0 =
  let addPrev :: Node -> FSM -> FSM
      addPrev n fsm = foldr (\r -> Map.adjust (over #prevs (r {to = n.id} :)) r.to) fsm n.nexts
   in foldr addPrev fsm0 fsm0

nodeP :: Parser Node
nodeP = do
  id <- lowerChar `someTill` char '{'
  rules <- ruleP `sepBy` char ',' <* char '}'
  pure $ Node id rules []

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

---------------------------------------------------------------------------
-- Dot

toDot :: FSM -> DotG String
toDot fsm =
  let (vs, es) = unzip $ map (\n -> (n.id, map (\r -> (n.id, r.to)) n.nexts)) (Map.elems fsm)
   in DotG vs (concat es)

---------------------------------------------------------------------------
-- https://adventofcode.com/2023/day/19

p :: String -> (FSM, [Part])
p = fromJust . parse

s0 :: String
s0 =
  unpack
    [trimming|
px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
|]

ss :: String
ss = unsafePerformIO $ loadDate 2023 19
{-# NOINLINE ss #-}
