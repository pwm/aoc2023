module AoC.Puzzles.Y2023D06Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D06
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 06)
    parse
    (solveA, 1660968)
    (solveB, 26499773)
