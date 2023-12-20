module AoC.Tests.Y2023D20Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D20
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 20)
    parse
    (solveA, 879834312)
    (solveB, ())
