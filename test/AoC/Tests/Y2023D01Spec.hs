module AoC.Tests.Y2023D01Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D01
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 01)
    parse
    (solveA, 54953)
    (solveB, 53868)
