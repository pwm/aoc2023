module AoC.Puzzles.Y2023D07Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D07
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 07)
    parse
    (solveA, 247815719)
    (solveB, 248747492)
