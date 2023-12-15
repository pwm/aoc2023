module AoC.Puzzles.Y2023D12 where

import AoC.Lib.Grid
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

solveA :: [(String, [Int])] -> ()
solveA _ = ()

solveB :: [(String, [Int])] -> ()
solveB _ = ()

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

ss :: String
ss = unsafePerformIO $ loadDate 2023 12
{-# NOINLINE ss #-}
