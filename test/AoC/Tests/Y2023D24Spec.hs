module AoC.Tests.Y2023D24Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D24
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 24)
    parse
    (solveA, 15318)
    (solveB, ())
