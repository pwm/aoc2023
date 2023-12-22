module AoC.Tests.Y2023D22Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D22
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 22)
    parse
    (solveA, 480)
    (solveB, ())
