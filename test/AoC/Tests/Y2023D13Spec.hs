module AoC.Tests.Y2023D13Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D13
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 13)
    parse
    (solveA, 35521)
    (solveB, 34795)
