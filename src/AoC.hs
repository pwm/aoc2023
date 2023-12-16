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
import AoC.Puzzles.Y2023D11 qualified as Y2023D11
import AoC.Puzzles.Y2023D12 qualified as Y2023D12
import AoC.Puzzles.Y2023D13 qualified as Y2023D13
import AoC.Puzzles.Y2023D14 qualified as Y2023D14
import AoC.Puzzles.Y2023D15 qualified as Y2023D15
import AoC.Puzzles.Y2023D16 qualified as Y2023D16
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
      ((2023, 10), mkSolverFor Y2023D10.parse Y2023D10.solveA Y2023D10.solveB),
      ((2023, 11), mkSolverFor Y2023D11.parse Y2023D11.solveA Y2023D11.solveB),
      ((2023, 12), mkSolverFor Y2023D12.parse Y2023D12.solveA Y2023D12.solveB),
      ((2023, 13), mkSolverFor Y2023D13.parse Y2023D13.solveA Y2023D13.solveB),
      ((2023, 14), mkSolverFor Y2023D14.parse Y2023D14.solveA Y2023D14.solveB),
      ((2023, 15), mkSolverFor Y2023D15.parse Y2023D15.solveA Y2023D15.solveB),
      ((2023, 16), mkSolverFor Y2023D16.parse Y2023D16.solveA Y2023D16.solveB)
    ]
