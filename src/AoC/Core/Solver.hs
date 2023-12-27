module AoC.Core.Solver
  ( Solutions,
    solvePuzzle,
    mkSolverFor,
    mkIOSolverFor,
  )
where

import AoC.Core.Date
import AoC.Core.File
import Data.Map.Strict (Map, (!?))
import GHC.Stats
import System.Exit (exitFailure)
import Prelude

type Solutions = Map (Int, Int) (Date -> IO ())

solvePuzzle :: Solutions -> Date -> IO ()
solvePuzzle solutions date =
  case solutions !? (getYear date.year, getDay date.day) of
    Nothing -> print ("Cannot find solution for " <> inputName date) >> exitFailure
    Just solverFor -> solverFor date

mkSolverFor ::
  (Show a, Show b) =>
  (String -> Maybe i) ->
  (i -> a) ->
  (i -> b) ->
  (Date -> IO ())
mkSolverFor parse solveA solveB date = do
  input <- getInput parse date
  let a = solveA input
      b = solveB input
  print (a, b) >> rtsStats

mkIOSolverFor ::
  (Show a, Show b) =>
  (String -> Maybe i) ->
  (i -> IO a) ->
  (i -> IO b) ->
  (Date -> IO ())
mkIOSolverFor parse solveA solveB date = do
  input <- getInput parse date
  a <- solveA input
  b <- solveB input
  print (a, b) >> rtsStats

getInput :: (String -> Maybe i) -> Date -> IO i
getInput parse date = do
  inputFile <- readInput date
  case parse inputFile of
    Nothing -> print ("Cannot parse " <> inputName date) >> exitFailure
    Just input -> pure input

rtsStats :: IO ()
rtsStats = do
  stats <- getRTSStats
  let cpuTime = stats.cpu_ns `div` 1e6 -- milliseconds
  putStrLn $ "Runtime: " <> show cpuTime <> "ms"
