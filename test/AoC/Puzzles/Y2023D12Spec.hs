module AoC.Puzzles.Y2023D12Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D12
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 12)
    parse
    (solveA, 6949)
    (solveB, ())
