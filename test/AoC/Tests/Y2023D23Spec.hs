module AoC.Tests.Y2023D23Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D23
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 23)
    parse
    (solveA, 2086)
    (solveB, 6526)
