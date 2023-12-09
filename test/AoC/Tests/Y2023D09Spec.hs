module AoC.Tests.Y2023D09Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D09
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 09)
    parse
    (solveA, 1916822650)
    (solveB, 966)
