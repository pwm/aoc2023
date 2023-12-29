module AoC.Tests.Y2023D18Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D18
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 18)
    parse
    (solveA, 76387)
    (solveB, ())
