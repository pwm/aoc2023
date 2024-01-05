module AoC.Puzzles.Y2023D11Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D11
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 11)
    parse
    (solveA, 9605127)
    (solveB, 458191688761)
