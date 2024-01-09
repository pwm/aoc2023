module AoC.Puzzles.Y2023D17Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D17
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 17)
    parse
    (solveA, ())
    (solveB, ())
