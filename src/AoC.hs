module AoC (aoc) where

import AoC.Core.ArgParser
import AoC.Core.Fetcher
import AoC.Core.Solver
import AoC.Lib.Prelude
import AoC.Puzzles.Y2023D01 qualified as Y2023D01
import AoC.Puzzles.Y2023D02 qualified as Y2023D02
import AoC.Puzzles.Y2023D03 qualified as Y2023D03
import AoC.Puzzles.Y2023D04 qualified as Y2023D04
import AoC.Puzzles.Y2023D05 qualified as Y2023D05
import AoC.Puzzles.Y2023D06 qualified as Y2023D06
import AoC.Puzzles.Y2023D07 qualified as Y2023D07
import AoC.Puzzles.Y2023D08 qualified as Y2023D08
import AoC.Puzzles.Y2023D09 qualified as Y2023D09
import AoC.Puzzles.Y2023D10 qualified as Y2023D10
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
      ((2023, 03), mkSolverFor Y2023D03.parse Y2023D03.solveA Y2023D03.solveB),
      ((2023, 04), mkSolverFor Y2023D04.parse Y2023D04.solveA Y2023D04.solveB),
      ((2023, 05), mkSolverFor Y2023D05.parse Y2023D05.solveA Y2023D05.solveB),
      ((2023, 06), mkSolverFor Y2023D06.parse Y2023D06.solveA Y2023D06.solveB),
      ((2023, 07), mkSolverFor Y2023D07.parse Y2023D07.solveA Y2023D07.solveB),
      ((2023, 08), mkSolverFor Y2023D08.parse Y2023D08.solveA Y2023D08.solveB),
      ((2023, 09), mkSolverFor Y2023D09.parse Y2023D09.solveA Y2023D09.solveB),
      ((2023, 10), mkSolverFor Y2023D10.parse Y2023D10.solveA Y2023D10.solveB)
    ]
