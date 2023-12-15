module AoC.Puzzles.Y2023D12 where

import AoC.Lib.Parser
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

parse :: String -> Maybe [(String, [Int])]
parse =
  traverse
    ( (\(a, b) -> (,) a <$> traverse readMaybe (splitOn "," b))
        <=< l2p . splitOn " "
    )
    . lines

-- 6949
solveA :: [(String, [Int])] -> Int
solveA = sum . map (\(s, ns) -> length $ filter (check ns) $ last $ levels $ bt s)

solveB :: [(String, [Int])] -> ()
solveB _ = ()

bt :: String -> Tree String
bt = unfoldTree $ \s ->
  (s, if '?' `elem` s then [replace '.' s, replace '#' s] else [])

check :: [Int] -> String -> Bool
check xs s =
  let f = map length . filter ('#' `elem`) . group
   in f s == xs

replace :: Char -> String -> String
replace _ [] = []
replace c (x : xs)
  | x == '?' = c : xs
  | otherwise = x : replace c xs

---------------------------------------------------------------------------
-- Test data

p :: String -> [(String, [Int])]
p = fromJust . parse

s0 :: String
s0 =
  unpack
    [trimming|
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
|]

-- load actual input from the downloaded file
ss :: String
ss = unsafePerformIO $ loadDate 2023 12
{-# NOINLINE ss #-}
