module AoC.Puzzles.Y2023D05Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D05
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 05)
    parse
    (solveA, 318728750)
    (solveB, 37384986)
