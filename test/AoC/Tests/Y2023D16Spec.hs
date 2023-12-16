module AoC.Tests.Y2023D16Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D16
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 16)
    parse
    (solveA, 8539)
    (solveB, 8674)
