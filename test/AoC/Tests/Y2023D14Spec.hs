module AoC.Tests.Y2023D14Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D14
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 14)
    parse
    (solveA, 106378)
    (solveB, 90795)
