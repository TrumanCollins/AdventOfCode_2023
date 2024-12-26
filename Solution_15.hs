-- For Advent of Code 2023
--
-- By Truman Collins
-- December 2023

module Solution_15 (
  puzzle_15
) where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char
import qualified Data.Map as M
import Control.Monad
import Utilities


-- Constants for this puzzle.

puzzNum :: Int
puzzNum =  15
inputFile :: String
inputFile = "puzzle_15.inp"

data Instruction = Instruction { _label :: String
                               , _box   :: Int
                               , _op    :: Operation
                               , _focal :: Int
                               } deriving Show

type BoxMap = M.Map Int Box

data Box = Box { _boxNumber :: Int
               , _lenses    :: BoxLenses
               } deriving Show

type BoxLenses = [LabeledLens]

type LabeledLens = (String, Int)

data Operation = Add | Remove deriving (Eq, Show)

emptyBoxes :: BoxMap
emptyBoxes = M.empty

--
-- Code for Puzzle 15.
--

puzzle_15 :: IO (Int, Int)
puzzle_15 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInput <- fmap (splitOn "," . filter (/= '\n')) (readFile inputFile)

  -- Check for errors in the input.

  let (errorInInput, errorMsg) = checkForInputErrors puzzInput
  when errorInInput
       (ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile errorMsg))

  let ansPart1 = (sum . map computeHash) puzzInput

  let instructionsMaybe = map decipherInstruction puzzInput

  when (any isNothing instructionsMaybe)
       (ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile "Unreadable instruction."))

  let instructions = catMaybes instructionsMaybe
      initialBoxes = emptyBoxes
      finalBoxes = foldl' doOperation initialBoxes instructions
      ansPart2 = (sum . map computeFocusingPower . M.toList) finalBoxes
  
  return (ansPart1, ansPart2)

computeFocusingPower :: (Int, Box) -> Int
computeFocusingPower (_, Box boxNumber lenses) = result
  where
    result = (sum . map (* (boxNumber + 1)) . zipWith (*) [1..] . reverse . map snd) lenses

doOperation :: BoxMap -> Instruction -> BoxMap
doOperation boxMap (Instruction label boxID op focal) = resultBoxMap
  where
    resultBoxMap = M.insert boxID newBox boxMap
    newBox = Box boxID modifiedLenses
    modifiedLenses = if changed || op == Remove then modifiedLensesInitial
                     else (label, focal) : modifiedLensesInitial
    (changed, modifiedLensesInitial) = doOpOnLensList lensesToModify
    lensesToModify = maybe [] _lenses currBoxMaybe
    currBoxMaybe = M.lookup boxID boxMap

    doOpOnLensList :: BoxLenses -> (Bool, BoxLenses)
    doOpOnLensList [] = (False, [])
    doOpOnLensList (currLens@(currLabel, _) : remainingLenses)
      | currLabel /= label = let (resultChanged, resultModLenses) = doOpOnLensList remainingLenses
                             in  (resultChanged, currLens : resultModLenses)
      | op == Remove = (True, remainingLenses)
      | otherwise = (True, (currLabel, focal) : remainingLenses)

computeHash :: String -> Int
computeHash = foldl' (\acc ch -> (acc + ord ch) * 17 `rem` 256) 0

decipherInstruction :: String -> Maybe Instruction
decipherInstruction [] = Nothing
decipherInstruction str
  | null label || null strRemainder = Nothing
  | opCh /= '-' && opCh /= '=' = Nothing
  | op == Remove && (not . null) focalStr = Nothing
  | op == Add && null focalStr = Nothing
  | not (all isDigit focalStr) = Nothing
  | otherwise = Just $ Instruction label box op focal
  where
    op = if opCh == '-' then Remove else Add
    (opCh : focalStr) = strRemainder
    focal = if op == Remove then 0 else read focalStr
    box = computeHash label
    (label, strRemainder) = span (\ch -> ch /= '-' && ch /= '=') str
    
-- Check for easy-to-spot errors in the input lines.

checkForInputErrors :: [String] -> (Bool, String)
checkForInputErrors [] = (True, "Null input.")
checkForInputErrors _ = (False, "")
