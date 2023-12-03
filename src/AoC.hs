module AoC (aoc) where

import AoC.Core.ArgParser
import AoC.Core.Fetcher
import AoC.Core.Solver
import AoC.Lib.Prelude
import AoC.Puzzles.Y2023D01 qualified as Y2023D01
import AoC.Puzzles.Y2023D02 qualified as Y2023D02
import AoC.Puzzles.Y2023D03 qualified as Y2023D03
import Data.Map.Strict qualified as Map

aoc :: IO ()
aoc =
  execParser opts >>= \case
    Fetch date -> fetchPuzzle date
    Solve date -> solvePuzzle solutions date

solutions :: Solutions
solutions =
  Map.fromList
    [ ((2023, 01), mkSolverFor Y2023D01.parse Y2023D01.solveA Y2023D01.solveB),
      ((2023, 02), mkSolverFor Y2023D02.parse Y2023D02.solveA Y2023D02.solveB),
      ((2023, 03), mkSolverFor Y2023D03.parse Y2023D03.solveA Y2023D03.solveB)
    ]
