module AoC.Puzzles.Y2023D03Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D03
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 03)
    parse
    (solveA, 536202)
    (solveB, 78272573)
