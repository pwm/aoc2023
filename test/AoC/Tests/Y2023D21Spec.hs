module AoC.Tests.Y2023D21Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D21
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 21)
    parse
    (solveA, 3853)
    (solveB, ())