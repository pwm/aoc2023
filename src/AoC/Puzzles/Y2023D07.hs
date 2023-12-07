module AoC.Puzzles.Y2023D07 where

import AoC.Lib.Parser
import AoC.Lib.Prelude

parse :: String -> Maybe [(String, Int)]
parse = parseMaybe (handP `sepEndBy` newline)

solveA :: [(String, Int)] -> Int
solveA = score . concatMap (sortBy (compCards orderA)) . groupHands

solveB :: [(String, Int)] -> Int
solveB xs =
  -- with the assumption that each hand have a unique bid
  let unJoker (_, i) = (fromJust (lookup i (map swap xs)), i)
   in score
        . concatMap
          (sortBy (compCards orderB) . map unJoker)
        . groupHands
        . map (first useJoker)
        $ xs

orderA, orderB :: String
orderA = "23456789TJQKA"
orderB = "J23456789TQKA"

score :: [(String, Int)] -> Int
score = sum . zipWith (\r (_, bid) -> r * bid) [1 ..]

useJoker :: String -> String
useJoker cs
  | cs /= "JJJJJ", r <- replace cs = map (\c -> if c == 'J' then r else c) cs
  | otherwise = "AAAAA"
  where
    replace = head . head . sortOn (Down . length) . group . sort . fst . partition (/= 'J')

compCards :: String -> (String, Int) -> (String, Int) -> Ordering
compCards order (c1, _) (c2, _) = go c1 c2
  where
    go :: String -> String -> Ordering
    go (a : as) (b : bs)
      | o <- compCard order a b = if o /= EQ then o else go as bs
    go _ _ = EQ

compCard :: String -> Char -> Char -> Ordering
compCard order a b = elemIndex a order `compare` elemIndex b order

groupHands :: [(String, Int)] -> [[(String, Int)]]
groupHands =
  groupBy (\(a, _) (b, _) -> handType a >= handType b)
    . sortOn (handType . fst)

data HandType = High | One | Two | Three | Full | Four | Five
  deriving stock (Show, Eq, Ord, Enum, Bounded)

handType :: String -> HandType
handType hand
  | five hand = Five
  | four hand = Four
  | full hand = Full
  | three hand = Three
  | two hand = Two
  | one hand = One
  | otherwise = High

five, four, full, three, two, one :: String -> Bool
five = (== 1) . length . nubOrd
four = (== [4, 1]) . rsort . map length . group . sort
full = (== [3, 2]) . rsort . map length . group . sort
three = (== [3, 1, 1]) . rsort . map length . group . sort
two = (== [2, 2, 1]) . rsort . map length . group . sort
one = (== 4) . length . nubOrd

handP :: Parser (String, Int)
handP = do
  hand <- count 5 alphaNumChar <* char ' '
  bid <- intP
  pure (hand, bid)
