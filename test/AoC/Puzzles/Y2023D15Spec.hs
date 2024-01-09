module AoC.Puzzles.Y2023D15Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D15
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 15)
    parse
    (solveA, 504449)
    (solveB, 262044)
