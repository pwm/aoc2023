module AoC.Puzzles.Y2023D08Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D08
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 08)
    parse
    (solveA, 21251)
    (solveB, 11678319315857)
