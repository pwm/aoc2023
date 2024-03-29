module AoC.Puzzles.Y2023D19Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2023D19
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2023 19)
    parse
    (solveA, 350678)
    (solveB, 124831893423809)
