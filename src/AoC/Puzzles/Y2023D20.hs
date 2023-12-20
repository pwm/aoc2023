{-# OPTIONS_GHC -Wno-partial-fields #-}

module AoC.Puzzles.Y2023D20 where

import AoC.Lib.Dot
import AoC.Lib.Parser
import AoC.Lib.Prelude hiding (id)
import Data.Map.Strict qualified as Map

parse :: String -> Maybe Circuit
parse = parseMaybe circuitP

-- 879834312
solveA :: Circuit -> Int
solveA c =
  let s = pushTill 1000 c
   in s.lows * s.highs

solveB :: Circuit -> ()
solveB _ = ()

pushTill :: Int -> Circuit -> S
pushTill n c = execState (timesM n (\_ -> bftM nexts ("button", Low)) ()) (S c 0 0)

data S = S
  { circuit :: Circuit,
    lows :: Int,
    highs :: Int
  }
  deriving stock (Show, Eq, Ord)

nexts :: (String, Pulse) -> State S [(String, Pulse)]
nexts (mid, pulse) = do
  s <- get
  let ms = lookups s.circuit (s.circuit ! mid).outs
  modify $ \s' ->
    if pulse == Low
      then s' {lows = s.lows + length ms}
      else s' {highs = s.highs + length ms}
  let res = mapMaybe (send mid pulse) ms
      c' = Map.fromList $ map ((\m -> (m.id, m)) . fst) res
  modify $ \s' -> s' {circuit = Map.union c' s.circuit}
  pure $ map (first (.id)) res

send :: String -> Pulse -> Module -> Maybe (Module, Pulse)
send from pulse m@Module {kind} = case kind of
  Button -> Just (m, pulse)
  Broadcaster -> Just (m, pulse)
  FlipFlop status -> case (pulse, status) of
    (Low, False) -> Just (m {kind = FlipFlop True}, High)
    (Low, True) -> Just (m {kind = FlipFlop False}, Low)
    (High, _) -> Nothing
  Conjunction ps ->
    let ps' = Map.insert from pulse ps
     in Just (m {kind = Conjunction ps'}, if null (Map.filter (== Low) ps') then Low else High)
  Output -> Nothing

bftM :: forall m n. (Monad m) => (n -> m [n]) -> n -> m ()
bftM nexts from = go [from]
  where
    go :: [n] -> m ()
    go [] = pure ()
    go (n : unseen) = do
      ns <- nexts n
      go (unseen <> ns)

type Circuit = Map String Module

data Module = Module
  { id :: String,
    kind :: Kind,
    ins :: [String],
    outs :: [String]
  }
  deriving stock (Show, Eq, Ord, Generic)

data Kind
  = Button
  | Broadcaster
  | FlipFlop Bool
  | Conjunction {mem :: Map String Pulse}
  | Output
  deriving stock (Show, Eq, Ord, Generic)

data Pulse = Low | High
  deriving stock (Show, Eq, Ord, Generic)

circuitP :: Parser Circuit
circuitP = do
  ms <- moduleP `sepEndBy` newline
  let addOuts :: Module -> Circuit -> Circuit
      addOuts m c = foldr (\mid -> Map.insert mid (Module mid Output [] [])) c m.outs
  let c0 = Map.singleton "button" (Module "button" Button [] ["broadcaster"])
  pure $ initConj $ addIns $ foldr (\m -> Map.insert m.id m) (foldr addOuts c0 ms) ms

moduleP :: Parser Module
moduleP = do
  kind <- try (FlipFlop False <$ char '%' <|> Conjunction mempty <$ char '&') <|> (Broadcaster <$ strP0 "broadcaster")
  id <- fromMaybe "broadcaster" <$> optional (try (some lowerChar))
  _ <- strP " ->"
  outs <- some lowerChar `sepBy` strP ","
  pure $ Module id kind [] outs

addIns :: Circuit -> Circuit
addIns c0 = foldr (\m c -> foldr (Map.adjust (over #ins (m.id :))) c m.outs) c0 c0

initConj :: Circuit -> Circuit
initConj = Map.map $ \case
  m@Module {kind = Conjunction {}} -> m {kind = Conjunction (Map.fromList $ map (,Low) m.ins)}
  m -> m

---------------------------------------------------------------------------
-- Dot

toDot :: Circuit -> DotG String
toDot c =
  let (vs, es) = unzip $ map (\m -> (m.id, map (m.id,) m.outs)) (Map.elems c)
   in DotG vs (concat es)

-- ddg "s0" $ p s0
ddg :: String -> Circuit -> IO ()
ddg s = ddd ("/Users/pwm/work/aoc2023/.local/day20" <> s <> ".dot") . toDot

---------------------------------------------------------------------------
-- https://adventofcode.com/2023/day/20

p :: String -> Circuit
p = fromJust . parse

s0, s1 :: String
s0 =
  unpack
    [trimming|
broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
|]
s1 =
  unpack
    [trimming|
broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output
|]

ss :: String
ss = unsafePerformIO $ loadDate 2023 20
{-# NOINLINE ss #-}
