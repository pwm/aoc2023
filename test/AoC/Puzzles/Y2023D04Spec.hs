module AoC.Puzzles.Y2023D04Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D04
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 04)
    parse
    (solveA, 21821)
    (solveB, 5539496)
