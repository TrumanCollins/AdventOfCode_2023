-- Solutions to Advent of Code 2022.
--
-- By Truman Collins
-- December 1, 2023 to sometime in 2024.
--
-- Note that in the timings for the two answers for a particular puzzle. The first one includes
-- parsing, and common calculation, and any computation done before final error checks in the IO
-- code.

import Control.Monad
import Control.DeepSeq
import System.IO (hFlush, stdout)
import System.Clock
import Text.Printf
import Utilities
import Solution_01
import Solution_02
import Solution_03
import Solution_04
import Solution_05
import Solution_06
import Solution_07
import Solution_08
import Solution_09
import Solution_10
import Solution_11
import Solution_12
import Solution_13
import Solution_14
import Solution_15
import Solution_16
import Solution_17
import Solution_18
import Solution_19
import Solution_20

-- The amount of space to allow for a result printed.

answerSpace :: Int
answerSpace = 20

-- The amount of space from the start of the line to the space after "Part 1:".

partSpace :: Int
partSpace = 18

-- Insure we can print strings without additional quotes.

newtype  NoQuotes = NoQuotes String
instance Show NoQuotes where show (NoQuotes str) = str

-- Helper functions for time calculations.

convertTimeToDouble :: TimeSpec -> Double
convertTimeToDouble tm = fromIntegral (sec tm) + fromIntegral (nsec tm) / 1.0e9

computeElapsedTime :: (TimeSpec, TimeSpec) -> Double
computeElapsedTime (startTime, endTime)
  = convertTimeToDouble endTime - convertTimeToDouble startTime

-- Check validity, based on known answer, and time running the solutions of two integer answers.
-- I used to compute and time both solutions before printing them, but found that if there was a
-- crash in part 2, the solution and time to part 1 was never printed. I then spread out the two
-- into separate computations and printing, which is not quite as clean, but functionally is
-- what I prefer.

computeCheckAndPrint :: (PrintfArg a, PrintfArg b, Show a, Show b, Eq a, Eq b, NFData a, NFData b)
                        => IO (a, b) -> Int -> Bool -> (a, b) -> IO ()
computeCheckAndPrint puzzleFn puzzleNumber printPart2 (correctAnsA, correctAnsB) = do

  -- Here we time the parse and error checking stage of the puzzle. Get the start and end times, and
  -- note that the two result values won't be computed now because of lazy evaluation. Print out the
  -- puzzle number and parsing time and flush this information before triggering the actual
  -- computations.

  startParseTime <- getTime Realtime
  (resultA, resultB) <- puzzleFn
  endParseTime <- getTime Realtime

  let puzzleNumStr = padFront 3 (show puzzleNumber)
      diffStrP = printf timeFormatStr (computeElapsedTime (startParseTime, endParseTime))
  
  putStr $ mconcat ["Puzzle", puzzleNumStr]
  putStrLn $ mconcat [" Parse time: ", padFront (answerSpace - 2) " ", "(", diffStrP, ")"]
  hFlush stdout

  -- Now trigger and time the computation of part 1, then format the result, time taken, and print
  -- it out.

  computeTimeAndPrintResult 1 resultA correctAnsA

  -- Now trigger and time the computation of part 2, when printing part 2 is called for.
  
  when printPart2
    (computeTimeAndPrintResult 2 resultB correctAnsB)

  where

    -- Trigger and time computation of the result, then format and print the result and timing of
    -- its computation.

    computeTimeAndPrintResult partNumber resultValue correctAns = do
      startTime <- getTime Realtime
      endTime   <- resultValue `deepseq` getTime Realtime

      let diffStr   = printf timeFormatStr (computeElapsedTime (startTime, endTime))
          resultStr = genResultString (resultValue, correctAns, diffStr)

      printResults partSpace ("Part " ++ show (partNumber :: Int) ++ ": ") resultStr

    -- Generate the resulting string from the result, correct answer and time difference.

    genResultString (result, corrAns, tDiffStr) = resultStr
      where
        resultStr = if result == corrAns
                    then let openParen = if tDiffStr !! 1 == '.' then "  (" else " ("
                         in  ansPadded ++ openParen ++ tDiffStr ++ ")"
                    else "Error: expected " ++ show corrAns ++ ", but computed "
                         ++ show result ++ " (" ++ tDiffStr ++ ")"
        ansPadded = padFront answerSpace (removeQuotes ansStr)
        ansStr    = show result

    -- Print the results and flush the stream.

    printResults :: Int -> String -> String -> IO ()
    printResults padSize str res = do
      putStrLn $ mconcat [padFront padSize str, res]
      hFlush stdout

    -- Remove the quote marks from the front and back if this string has them.

    removeQuotes :: String -> String
    removeQuotes [] = []
    removeQuotes [x] = [x]
    removeQuotes str@(x : xs)
      | x /= '"' = str
      | last xs == '"' = init xs
      | otherwise = str

    -- Used to print a time value .

    timeFormatStr = "%0.5f sec"

--
-- Functions for the individual puzzles.
--

main :: IO ()
main = do

  startTime <- getTime Realtime

  -- Generate the results for each problem and check for the expected answer. The result will
  -- contain not only the result, but the time taken to compute it.

  computeCheckAndPrint puzzle_01  1 True (55130, 54985)
  computeCheckAndPrint puzzle_02  2 True (2164, 69929)
  computeCheckAndPrint puzzle_03  3 True (525181, 84289137)
  computeCheckAndPrint puzzle_04  4 True (21105, 5329815)
  computeCheckAndPrint puzzle_05  5 True (662197086, 52510809)
  computeCheckAndPrint puzzle_06  6 True (512295, 36530883)
  computeCheckAndPrint puzzle_07  7 True (247961593, 248750699)
  computeCheckAndPrint puzzle_08  8 True (12599, 8245452805243)
  computeCheckAndPrint puzzle_09  9 True (2038472161, 1091)
  computeCheckAndPrint puzzle_10 10 True (6831, 305)
  computeCheckAndPrint puzzle_11 11 True (9274989, 357134560737)
  computeCheckAndPrint puzzle_12 12 True (7286, 25470469710341)
  computeCheckAndPrint puzzle_13 13 True (32723, 34536)
  computeCheckAndPrint puzzle_14 14 True (109661, 90176)
  computeCheckAndPrint puzzle_15 15 True (503154, 251353)
  computeCheckAndPrint puzzle_16 16 True (8021, 8216)
  computeCheckAndPrint puzzle_17 17 True (694, 829)
  computeCheckAndPrint puzzle_18 18 False (40761, 42)
  computeCheckAndPrint puzzle_19 19 True (492702, 138616621185978)
  computeCheckAndPrint puzzle_20 20 False (680278040, 42)

  -- Report on the time taken by all of the solutions together.

  endTime <- getTime Realtime
  let diff = computeElapsedTime (startTime, endTime)
      diffStr = printf "%0.5f sec" diff

  putStrLn $ mconcat ["\nTotal time for all solutions: ", diffStr]
