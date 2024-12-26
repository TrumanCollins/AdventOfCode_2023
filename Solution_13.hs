-- For Advent of Code 2023
--
-- By Truman Collins
-- December 2023

module Solution_13 (
  puzzle_13
) where

import Data.Maybe
import Data.Either
import qualified Data.Array.Unboxed as UA
import Control.Monad
import Utilities


data Contents = Empt | Rock deriving (Eq, Ord, Show)

data Orientation = VertMirror | HorizMirror deriving (Eq, Show)

data MatchType = ExactReflection | OneErrorReflection deriving (Eq, Show)

type Location = (Int, Int)
type RowOrCol = Int

type OrientAndLoc = (Orientation, RowOrCol)

type TerrainArr = UA.Array Location Contents

-- Constants for this puzzle.

puzzNum :: Int
puzzNum =  13
inputFile :: String
inputFile = "puzzle_13.inp"

--
-- Code for Puzzle 13.
--

puzzle_13 :: IO (Int, Int)
puzzle_13 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInput <- fmap lines (readFile inputFile)

  -- Split the input into groups of related lines. Also check for errors in these groups.
  
  let inputLineGroups = splitGroups puzzInput
      errorChecks = (filter fst . map checkForInputErrors) inputLineGroups

  unless (null errorChecks)
    (ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile ((snd . head) errorChecks)))

  -- Now generate the two-dimensional arrays associated with this input, and check for errors,
  -- reporting the first if there aare any errors. This will do some error checking too, so look for
  -- these also.

  let mapOrigin = (1, 1)
      arrsOrError = map (construct2DUArrayFromInputStrings UpperLeft mapOrigin convSymbol)
                    inputLineGroups
      arrErrors = filter isLeft arrsOrError

  unless (null arrErrors)
    (let errorCode = fromLeft UnknownError (head arrErrors)
         errorMessage = correspondingArrCreationErrorMessage errorCode
     in  ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile errorMessage))

  -- If no errors were found, extract the list of arrays representing the input.

  let mapArrays = map (fromRight undefined) arrsOrError

  -- Now, find the mirror orientations and locations for each array.
  
  let mirrorLocsAndDirsMa = map (findMirrorOrientAndLoc ExactReflection) mapArrays

  when (any isNothing mirrorLocsAndDirsMa)
    (ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile "No mirror location found."))

  let mirrorLocsAndDirs = catMaybes mirrorLocsAndDirsMa
      ansPart1 = (sum . map score) mirrorLocsAndDirs

  let mirrorLocsAndDirsMa2 = map (findMirrorOrientAndLoc OneErrorReflection) mapArrays
  
  when (any isNothing mirrorLocsAndDirsMa2)
    (ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile "No mirror location found part 2."))

  let mirrorLocsAndDirs2 = catMaybes mirrorLocsAndDirsMa2
      ansPart2 = (sum . map score) mirrorLocsAndDirs2

  return (ansPart1, ansPart2)

-- Compute the score for part 1 for an array of terrain.

score :: OrientAndLoc -> Int
score (VertMirror, val)  = val
score (HorizMirror, val) = 100 * val

findMirrorOrientAndLoc :: MatchType -> TerrainArr -> Maybe OrientAndLoc
findMirrorOrientAndLoc matchType arr
  | exactlyOneReflection = if lengthIsOne vertReflections
                           then Just (VertMirror, (fst . head) vertReflections)
                           else Just (HorizMirror, (fst . head) horizReflections)
  | otherwise = Nothing
  where

    -- Determine the horizontal and vertical mirrors and see if there is just one.

    exactlyOneReflection = lengthIsOne (vertReflections ++ horizReflections)
    vertReflections  = filter reflectionCheckFn verticalMirrorDiffs
    horizReflections = filter reflectionCheckFn horizontalMirrorDiffs

    -- The check function we want to use based on the type of reflection we are looking for.

    reflectionCheckFn :: (Int, [(Location, Location)]) -> Bool
    reflectionCheckFn = if matchType == ExactReflection
                        then perfectReflection else oneErrorReflection

    -- Tests for a perfect reflection and for a reflection with exactly one error.

    perfectReflection :: (Int, [(Location, Location)]) -> Bool
    perfectReflection = null . snd

    oneErrorReflection :: (Int, [(Location, Location)]) -> Bool
    oneErrorReflection (_, xs) = lengthIsOne xs

    -- Return true if the length of this list is one. Saves computing the actual length of the list,
    -- which can be expensive.

    lengthIsOne :: [a] -> Bool
    lengthIsOne [] = False
    lengthIsOne [_] = True
    lengthIsOne _ = False

    verticalMirrorDiffs   = map (genNonIdenticalPairs genPairsForVertMirror) [xMin..(xMax - 1)]
    horizontalMirrorDiffs = map (genNonIdenticalPairs genPairsForHorizMirror) [yMin..(yMax - 1)]

    -- Find non-identical location pairs for a given mirror location.

    genNonIdenticalPairs :: (Int -> [(Location, Location)]) -> Int -> (Int, [(Location, Location)])
    genNonIdenticalPairs genPairsForMirror i = (i, foldr weedOutIdentical [] (genPairsForMirror i))

    weedOutIdentical :: (Location, Location) -> [(Location, Location)] -> [(Location, Location)]
    weedOutIdentical locs@(loc1, loc2) acc
      | (arr UA.! loc1) == (arr UA.! loc2) = acc
      | otherwise = locs : acc

    -- These two functions will generate all pair locations to compare for a row or column as
    -- specified. The row or column is considered the closest one to the mirror on the low index
    -- side.

    genPairsForVertMirror :: Int -> [(Location, Location)]
    genPairsForVertMirror = allMirrorPairs (,) (xMin, xMax) (yMin, yMax)

    genPairsForHorizMirror :: Int -> [(Location, Location)]
    genPairsForHorizMirror = allMirrorPairs (flip (,)) (yMin, yMax) (xMin, xMax)

    -- This function will take a column or row number, and generate a list of all terrain array
    -- locations to compare, assuming that the mirror is between this column or row and the next
    -- one. It also takes a low and high bound for the columns or rows, as appropriate, and secondly
    -- the pair of other bounds. The first parameter is a function that takes a column or row index
    -- and the other index secondly and returns a location. These parameters allow this function to
    -- work for both horizontal and vertical mirrors.

    allMirrorPairs :: (Int -> Int -> Location) -> (Int, Int) -> (Int, Int) -> Int
                            -> [(Location, Location)]
    allMirrorPairs toLocFn (low1Bound, high1Bound) (low2Bound, high2Bound) lineInd = locsToCompare
      where

        locsToCompare = concatMap genLineLocs linePairs

        genLineLocs :: (Int, Int) -> [(Location, Location)]
        genLineLocs (line1Ind, line2Ind)
          = [(toLocFn line1Ind i, toLocFn line2Ind i) | i <- [low2Bound..high2Bound]]

        -- These are the pairs of line indexes (horizontal or vertical) that need to be compared.

        linePairs = (take lineCount . iterate (\(x, y) -> (x - 1, y + 1))) (lineInd, lineInd + 1)

        -- Figure out how many terain lines we need to compare.

        lineCount     = min hereDownCount hereUpCount
        hereDownCount = lineInd - low1Bound + 1
        hereUpCount   = high1Bound - lineInd

    -- We need the bounds of the array to search for mirrors.

    ((xMin, yMin), (xMax, yMax)) = UA.bounds arr

-- Split the series of line strings into groups based on empty lines.

splitGroups :: [String] -> [[String]]
splitGroups xs
  | null tailLinesDropped = [headLines]
  | otherwise = headLines : splitGroups tailLinesDropped
  where
   tailLinesDropped = dropWhile null tailLines
   (headLines, tailLines) = break null xs

-- Check for easy-to-spot errors in the input lines.

checkForInputErrors :: [String] -> (Bool, String)
checkForInputErrors inputStrings
  | null inputStrings = (True, "Empty input.")
  | (not . all (== firstLen)) strLengths = (True, "Input string lengths are not equal.")
  | all (\ch -> ch == '.' || ch == '#') (concat inputStrings) = (False, "")
  | otherwise = (True, "Non-Alphanumeric characters found.")
  where
    strLengths = map length inputStrings
    firstLen   = head strLengths

convSymbol :: Char -> Maybe Contents
convSymbol ch = case ch of
             '.' -> Just Empt
             '#' -> Just Rock
             _   -> Nothing
