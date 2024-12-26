-- For Advent of Code 2023
--
-- By Truman Collins
-- December 2023

module Solution_11 (
  puzzle_11
) where

import Data.Either
import qualified Data.Array.Unboxed as UA
import Control.Monad
import Utilities


-- Constants for this puzzle.

puzzNum :: Int
puzzNum =  11
inputFile :: String
inputFile = "puzzle_11.inp"

-- Types defined for this puzzle.

data MapElem = Galaxy | Empt deriving (Eq, Show)

type Location = (Int, Int)
type MapArray = UA.Array Location MapElem
type EmptyRCArr = UA.Array Int Int

--
-- Code for Puzzle 11.
--

puzzle_11 :: IO (Int, Int)
puzzle_11 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInput <- fmap lines (readFile inputFile)

  -- Read in the array of galaxy locations, converting to an enum for each element and error checking
  -- as well.

  let mapOrigin = (0, 0)
      mapArrOrError = construct2DUArrayFromInputStrings LowerLeft mapOrigin convSymbol puzzInput

  -- Check for errors in the input.

  when (isLeft mapArrOrError)
    (let errorCode = fromLeft UnknownError mapArrOrError
         errorMessage = correspondingArrCreationErrorMessage errorCode
     in  ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile errorMessage))

  -- Here we have the map of cities and empty locations in an unboxed 2-dimensional array.

  let mapArr = fromRight undefined mapArrOrError

  let emptyColAndRowCounts = findEmptyRCs mapArr

  let galaxyLocsOrig = (map fst . filter ((== Galaxy) . snd) . UA.assocs) mapArr
      ansPart1 = sumAllGalaxyPairDists emptyColAndRowCounts 1 0 galaxyLocsOrig
      ansPart2 = sumAllGalaxyPairDists emptyColAndRowCounts 999999 0 galaxyLocsOrig

  return (ansPart1, ansPart2)

-- Search for empty rows and empty columns in the map and return a pari holding an array of the
-- number of empty columns to the left of the current index, and the number of empty rows above the
-- current index.

findEmptyRCs :: MapArray -> (EmptyRCArr, EmptyRCArr)
findEmptyRCs mapArr = (emptyColArr, emptyRowArr)
  where
    emptyColArr = UA.array (xLow, xHigh) (zip [xLow..] initColList)
    emptyRowArr = UA.array (yLow, yHigh) (zip [yLow..] initRowList)
    initRowList = genCounts 0 (checkRowFn, [xLow..xHigh]) [yLow..yHigh]
    initColList = genCounts 0 (checkColFn, [yLow..yHigh]) [xLow..xHigh]

    genCounts :: Int -> (Int -> Int -> MapElem, [Int]) -> [Int] -> [Int]
    genCounts _ _ [] = []
    genCounts currEmpties checkFnRng@(checkFn, rcRange) (currInd : inds)
      | currIsEmpty = let newCurrEmpties = currEmpties + 1
                      in  newCurrEmpties : genCounts newCurrEmpties checkFnRng inds
      | otherwise = currEmpties : genCounts currEmpties checkFnRng inds
      where
        currIsEmpty = all ((== Empt) . checkFn currInd) rcRange

    checkRowFn :: Int -> Int -> MapElem
    checkRowFn yInd xInd = mapArr UA.! (xInd, yInd)

    checkColFn :: Int -> Int -> MapElem
    checkColFn xInd yInd = mapArr UA.! (xInd, yInd)

    ((xLow, yLow), (xHigh, yHigh)) = UA.bounds mapArr

-- Sum all of the distances between all pairs of cities.

sumAllGalaxyPairDists :: (EmptyRCArr, EmptyRCArr) -> Int -> Int -> [Location] -> Int
sumAllGalaxyPairDists _ _ acc [] = acc
sumAllGalaxyPairDists (emptyColCounts, emptyRowCounts) gapExtra acc (galaxy : galaxies)
  = sumAllGalaxyPairDists (emptyColCounts, emptyRowCounts) gapExtra accWithThisGalaxy galaxies
  where
    accWithThisGalaxy = ((+ acc) . sum . map (compDist galaxy)) galaxies

    -- Compute the distance between the two galaxies at these locations taking into account the
    -- number of empty rows and columns between them as well as the expansion distance from an empty
    -- row or column.

    compDist :: Location -> Location -> Int
    compDist (x1, y1) (x2, y2) = dist
      where
        dist = colsOnOrigMap + rowsOnOrigMap + emptyColsBetw * gapExtra + emptyRowsBetw * gapExtra
        colsOnOrigMap = abs (x1 - x2)
        emptyColsBetw = abs ((emptyColCounts UA.! x1) - (emptyColCounts UA.! x2))
        rowsOnOrigMap = abs (y1 - y2)
        emptyRowsBetw = abs ((emptyRowCounts UA.! y1) - (emptyRowCounts UA.! y2))

-- Convert an input character to the type of map element.

convSymbol :: Char -> Maybe MapElem
convSymbol ch = case ch of
                  '.' -> Just Empt
                  '#' -> Just Galaxy
                  _   -> Nothing

