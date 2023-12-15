module AoC.Puzzles.Y2023D15 where

import AoC.Lib.Parser
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map

parse :: String -> Maybe [Cmd]
parse = parseMaybe ((cmdP `sepBy` char ',') <* optional newline)

solveA :: [Cmd] -> Int
solveA = sum . map (hash . ppOp . (.op))

solveB :: [Cmd] -> Int
solveB = checksum . run

data Op
  = Del String
  | Add String Int
  deriving stock (Show, Eq, Ord, Generic)

data Cmd = Cmd
  { slot :: Int,
    op :: Op
  }
  deriving stock (Show, Eq, Ord, Generic)

type HM = Map Int [(String, Int)]

checksum :: HM -> Int
checksum =
  Map.foldlWithKey' checksum1 0
    . Map.mapKeys (+ 1)
    . Map.map (Map.fromList . zip [1 ..])
  where
    checksum1 :: Int -> Int -> Map Int (String, Int) -> Int
    checksum1 acc k v =
      let boxVal = sum $ map ((\(a, (b, c)) -> a * b * c) . (k,) . second snd) $ Map.toList v
       in acc + boxVal

run :: [Cmd] -> HM
run = foldl' run1 mempty
  where
    run1 :: HM -> Cmd -> HM
    run1 hm = \case
      Cmd {slot, op = Del lbl} -> Map.adjust (filter ((/= lbl) . fst)) slot hm
      Cmd {slot, op = Add lbl n} -> case hm !? slot of
        Nothing -> Map.insert slot [(lbl, n)] hm
        Just l -> case lookup lbl l of
          Nothing -> Map.insert slot (l <> [(lbl, n)]) hm
          Just _ -> Map.adjust (map (\(s, i) -> (s, if s == lbl then n else i))) slot hm

hash :: String -> Int
hash =
  let hashc c = modify $ \x -> (x + ord c) * 17 `mod` 256
   in flip execState 0 . traverse hashc

cmdP :: Parser Cmd
cmdP = do
  lbl <- some letterChar
  op <- (Del lbl <$ char '-') <|> (char '=' *> (Add lbl <$> intP))
  pure $ Cmd (hash lbl) op

ppOp :: Op -> String
ppOp = \case
  Del lbl -> lbl <> "-"
  Add lbl n -> lbl <> "=" <> show n
