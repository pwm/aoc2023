module AoC.Tests.Y2023D02Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D02
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 02)
    parse
    (solveA, 2406)
    (solveB, 78375)
