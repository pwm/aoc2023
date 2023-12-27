module AoC.Tests.Y2023D25Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D25
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  iotester
    (mkDate 2023 25)
    parse
    (solveA, 601344)
    (solveB, ())
