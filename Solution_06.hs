-- For Advent of Code 2023
--
-- By Truman Collins
-- December 2023

module Solution_06 (
  puzzle_06
) where

import Control.Monad
import Utilities

data Race = Race { _time       :: Int
                 , _recDist    :: Int
                 , _winCount   :: Int
                 } deriving Show

-- Constants for this puzzle.

puzzNum :: Int
puzzNum =  6
inputFile :: String
inputFile = "puzzle_06.inp"

--
-- Code for Puzzle 06.
--

puzzle_06 :: IO (Int, Int)
puzzle_06 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInput <- fmap lines (readFile inputFile)

  -- Check for errors in the input.

  let (errorInInput, races, errorMsg) = checkForInputErrorsAndParse puzzInput
  when errorInInput
       (ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile errorMsg))

  -- For part 1, just compute the results for each race, and take the product.

  let ansPart1 = (product . map _winCount) races

  let timeP2 = (read . concatMap (show . _time)) races :: Int
      recDistP2 = (read . concatMap (show . _recDist)) races :: Int
      ansPart2 = countWinningWaits timeP2 recDistP2

  return (ansPart1, ansPart2)

-- Check for easy-to-spot errors in the input lines.

checkForInputErrorsAndParse :: [String] -> (Bool, [Race], String)
checkForInputErrorsAndParse inputStrings
  | length inputStrings < 2 = (True, [], "Fewer than 2 input lines.")
  | length inputStrings > 2 = (True, [], "More than 2 input lines.")
  | "Time:" /= timeWord = (True, [], "First line must begin with 'Time:'.")
  | "Distance:" /= distWord = (True, [], "Second line must begin with 'Distance:'.")
  | otherwise = (False, races, "")
  where
    races = zipWith genRaceData times distances
    times = (map read . words) timeStr
    distances = (map read . words) distStr
    (timeWord, timeStr) = (splitAt 5 . head) inputStrings
    (distWord, distStr) = (splitAt 9 . head . tail) inputStrings

-- Given a time and a record distance, return a Race record that includes those two pieces of
-- intormation along with all of the race results for pressing the button 0 to time milliseconds.

genRaceData :: Int -> Int -> Race
genRaceData time recDist = Race time recDist winCount
  where
    winCount = countWinningWaits time recDist

-- Calculate the score given the total time for the race and the time pushing the button at start.

calcScore :: Int -> Int -> Int
calcScore totalTime waitTime
  | waitTime < 0 || waitTime > totalTime
    = error (genErrStrForErrorFn puzzNum inputFile "Wait time out of bounds.")
  | otherwise = runTime * speed
  where
    runTime = totalTime - waitTime
    speed = waitTime

-- Compute the number of wait times that result in a score larger than the record distance. The
-- shape of this curve is an inverted parabola with Y-intercepts at 0 and totalTime. We can use a
-- binary search to find the points on both sides where we win.

countWinningWaits :: Int -> Int -> Int
countWinningWaits totalTime recDist

  -- Weird unexpected case, and 0 should be the answer.

  | totalTime < 0 = 0

  -- If the current record distance is less than 0, then all wait times are winning and since 0 is
  -- an option, there are totalTime + 1.

  | recDist < 0 = totalTime + 1

  -- If the best possible score is not greater than the record distance, then there are no winning
  -- waits.

  | midDist <= recDist = 0

  -- Now, here is the case where we do a binary search on each side of the parabola to see where the
  -- first and last wait times are that still result in a record distance. These points and all of
  -- the points inbetween them will be winning also, so count them.

  | otherwise = maxWinningWait - minWinningWait + 1
  where
    minWinningWait = binSearchJustPos (0, 0) (midWait, midDist)
    maxWinningWait = binSearchJustPos (midWait, midDist) (totalTime, 0)
    midWait = totalTime `quot` 2
    midDist = calcScore totalTime midWait

    -- A precondition is that the wait values are different and one of the assiciated values is
    -- below or equal to the record distance and the other is above.

    binSearchJustPos :: (Int, Int) -> (Int, Int) -> Int
    binSearchJustPos (lowWait, lowDist) (highWait, highDist)
      | highWait == lowWait + 1 = if lowDist > recDist then lowWait else highWait
      | midDistBS > recDist = if lowDist <= recDist
                              then binSearchJustPos (lowWait, lowDist) (midWaitBS, midDistBS)
                              else binSearchJustPos (midWaitBS, midDistBS) (highWait, highDist)
      | otherwise = if lowDist <= recDist
                    then binSearchJustPos (midWaitBS, midDistBS) (highWait, highDist)
                    else binSearchJustPos (lowWait, lowDist) (midWaitBS, midDistBS)
      where
        midWaitBS = (lowWait + highWait) `quot` 2
        midDistBS = calcScore totalTime midWaitBS
