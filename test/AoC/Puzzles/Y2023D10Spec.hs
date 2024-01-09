module AoC.Puzzles.Y2023D10Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D10
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 10)
    parse
    (solveA, 6838)
    (solveB, 451)
