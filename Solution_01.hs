-- For Advent of Code 2023
--
-- By Truman Collins
-- December 2023

module Solution_01 (
  puzzle_01
) where

import Data.List
import Data.Char
import Control.Monad
import Utilities

-- The first part was very straightforward. The second part was similar, but I mis-understood
-- whether "eightwo9" converted into "8wo9" or "829", and I assumed the former, when it was
-- the latter, so it took a while to think to try the other way.

-- Constants for this puzzle.

puzzNum :: Int
puzzNum =  1
inputFile :: String
inputFile = "puzzle_01.inp"

--
-- Code for Puzzle 1.
--

puzzle_01 :: IO (Int, Int)
puzzle_01 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  calibrationStrs <- fmap lines (readFile inputFile)

  -- Check for errors in the input.

  let (errorInInput, errorMsg) = checkForInputErrors calibrationStrs
  when errorInInput
       (ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile errorMsg))

  -- Compute the answer to part 1.

  let ansPart1 = (sum . map readCalibrationValue) calibrationStrs

  -- Compute the answer to part 2.

  let ansPart2 = (sum . map (readCalibrationValue . wordSubst)) calibrationStrs

  return (ansPart1, ansPart2)

-- Find the first and last digits by first filtering out all the characters that aren't digits, then
-- just taking the first and last of what is left and combining them into a two-digit integer.

readCalibrationValue :: String -> Int
readCalibrationValue str
  | null onlyDigits = error ("Puzzle 01: No digits present in input string: " ++ str)
  | otherwise = let firstChar  = head onlyDigits
                    secondChar = last onlyDigits
                in  read [firstChar, secondChar]
  where
    onlyDigits = filter isDigit str

-- Substitute the digit for the name of the digit in the string given the list of (name, digit)
-- pairs.

wordSubst :: String -> String
wordSubst [] = []
wordSubst str = ch : wordSubst chs
  where
    (ch : chs) = substDigForDigName digitNameList str

    substDigForDigName :: [(String, Char)] -> String -> String
    substDigForDigName [] s = s
    substDigForDigName ((currDigName, currDig) : digNamesRest) s
      | currDigName `isPrefixOf` s = currDig : drop 1 s
      | otherwise = substDigForDigName digNamesRest s

-- List of word substitutions. Included is the word for the digit, the character digit.

digitNameList :: [(String, Char)]
digitNameList = [("one", '1'), ("two", '2'), ("three", '3'), ("four", '4'), ("five", '5'),
                 ("six", '6'), ("seven", '7'), ("eight", '8'), ("nine", '9')]

-- Check for easy-to-spot errors in the input lines.

checkForInputErrors :: [String] -> (Bool, String)
checkForInputErrors inputStrings
  | all isAlphaNum (concat inputStrings) = (False, "")
  | otherwise = (True, "Non-Alphanumeric characters found.")
