-- For Advent of Code 2023
--
-- By Truman Collins
-- December 2023

module Solution_03 (
  puzzle_03
) where

import Data.List
import Data.Function
import Data.Char
import Data.Maybe
import Data.Either
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import Control.Monad
import Utilities

data SchematicData = SchematicData { _origChar :: Char
                                   , _partNum  :: Maybe (Int, SchematicLoc)
                                   } deriving Show

data SchematicInfo = SchematicInfo { _initList :: [(SchematicLoc, SchematicData)]
                                   , _symbols  :: [SymbolAndLoc]
                                   } deriving Show

type SchematicLoc = (Int, Int)
type SymbolAndLoc = (Char, SchematicLoc)
type SchematicCharArr = UA.Array SchematicLoc Char
type SchematicArr = UA.Array SchematicLoc SchematicData

-- Constants for this puzzle.

puzzNum :: Int
puzzNum =  3
inputFile :: String
inputFile = "puzzle_03.inp"

--
-- Code for Puzzle 03.
--

puzzle_03 :: IO (Int, Int)
puzzle_03 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInput <- fmap lines (readFile inputFile)

  let schematicOrigin = (0, 0)
      schematicArrOrError = construct2DUArrayFromInputStrings LowerLeft schematicOrigin checkSymbol
                            puzzInput

  -- Check for errors in the input.

  when (isLeft schematicArrOrError)
    (let errorCode = fromLeft UnknownError schematicArrOrError
         errorMessage = correspondingArrCreationErrorMessage errorCode
     in  ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile  errorMessage))

  -- Convert the character array that was read in into a more informative array that has more useful
  -- data about the numbers. Also harvest a list of all of the symbols that are not periods.

  let schematicCharArr = fromRight undefined schematicArrOrError
      (schematicArr, symbolList) = buildSchematicArray schematicCharArr

  -- The answer to the first part involves taking all of the symbol locations, finding all of their
  -- surrounding locations that are in bounds, taking any numbers corresponding to those areas along
  -- with their starting locations, sorting by those starting locations, and eliminating duplicate
  -- numbers based on those starting locations, then summing the remaining unique numbers.

  let ansPart1 = (sum . uniquefyAndFst . mapMaybe (_partNum . (schematicArr A.!))
                  . concatMap (surroundingLocs schematicArr . snd)) symbolList

  -- Filter out all the symbols except '*', then find the surrounding locations. Identify the
  -- numbers on these surrounding locations and make sure we have them uniquely. If and only if
  -- there are two of them adjacent to a '*', multiply them together. Then sum all of these
  -- products.

  let ansPart2 = (sum . map product . filter ((== 2) . length) . map (uniquefyAndFst
                  . mapMaybe (_partNum . (schematicArr A.!)) . surroundingLocs schematicArr . snd)
                  . filter ((== '*') . fst)) symbolList

  return (ansPart1, ansPart2)

-- Uniquefy and take first.

uniquefyAndFst :: [(Int, SchematicLoc)] -> [Int]
uniquefyAndFst = map (fst . head) . groupBy ((==) `on` snd) . sortBy (compare `on` snd)

-- Build a schematic array from the char array read in. This new array will have information about
-- the numbers that is more useful than the individual digits. Also, return the list of symbols and
-- their locations.

buildSchematicArray :: SchematicCharArr -> (SchematicArr, [SymbolAndLoc])
buildSchematicArray charArr = (schematicArr, symbolList)
  where
    schematicArr = A.array schematicBounds initList
    (SchematicInfo initList symbolList) = foldr combineRows (SchematicInfo [] []) infoByRows
    infoByRows = map (genSchematicInfo charArr) rowsOfLocs
    rowsOfLocs = map (\y -> [(x, y) | x <- [xLow..xHigh]]) [yLow..yHigh]
    schematicBounds@((xLow, yLow), (xHigh, yHigh)) = UA.bounds charArr

-- Generate the surrounding locations for a particular array index.

surroundingLocs :: A.Array SchematicLoc a -> SchematicLoc -> [SchematicLoc]
surroundingLocs currArray (x, y) = filter inBounds possibleSurroundingLocs
  where
    possibleSurroundingLocs = [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1),
                               (x, y - 1), (x, y), (x, y + 1),
                               (x + 1, y - 1), (x + 1, y), (x + 1, y + 1)]
    ((xLow, yLow), (xHigh, yHigh)) = A.bounds currArray
    inBounds :: SchematicLoc -> Bool
    inBounds (x', y')
      | x' < xLow || x' > xHigh || y' < yLow || y' > yHigh = False
      | otherwise = True

-- Nothing really to check for the characters read in.

checkSymbol :: Char -> Maybe Char
checkSymbol = Just

-- Combines a current SchematicInfo for a row with the accumulated one for the remaining rows. Used
-- by foldr.

combineRows :: SchematicInfo -> SchematicInfo -> SchematicInfo
combineRows (SchematicInfo currInit currSymbols) (SchematicInfo accInit accSymbols)
  = SchematicInfo (currInit ++ accInit) (currSymbols ++ accSymbols)

-- Walk through the character array that was read in and identify all of the numbers, their
-- locations, and all of the symbols and where they are. This can then be used to create a more
-- useful array to use to answer the problems.

genSchematicInfo :: SchematicCharArr -> [SchematicLoc] -> SchematicInfo
genSchematicInfo schematicCharArr = go Nothing
  where
    xHigh = (fst . snd . UA.bounds) schematicCharArr
    ord0 = ord '0'

    go :: Maybe (Int, Int, Int) -> [SchematicLoc] -> SchematicInfo
    go accValM locsRestOfRow

      -- We are done with this row. If we aren't in the middle of a number, then just return two
      -- null lists, otherwise the symbol list is null, but the initialization list will hold an
      -- entry for each digit in the number that we have just come to the end of by coming to the
      -- end of the row.

      | null locsRestOfRow = if isNothing accValM then SchematicInfo [] []
                             else SchematicInfo (genScannedValueEntries xHigh) []

      -- Here we have encountered a digit. If we aren't in the middle of reading a number then begin
      -- that process by saving the value and x and y coordinates of the first digit. If we are
      -- reading one, then incorporate this digit into the accumulating value.

      | isDigit ch = if isNothing accValM then go (Just (digVal, x, y)) remainingLocsInRow
                     else let newValue = currVal * 10 + digVal
                          in  newValue `seq` go (Just (newValue, xStart, currY)) remainingLocsInRow

      -- If we have found a non-digit symbol, then record it and its location on the symbol list if
      -- it isn't a '.'. If we have been keeping track of the digits of a number, we must have come
      -- to the end of it, so record the entries for that number before the entry for the current
      -- character.

      | otherwise
        = let nonDigitInitAndFurther = (loc, SchematicData ch Nothing) : furtherInit
              completedNumInit = genScannedValueEntries (x - 1)
              symbolList = if ch == '.' then furtherSymbols else (ch, loc) : furtherSymbols
              SchematicInfo furtherInit furtherSymbols = go Nothing remainingLocsInRow
              initList = if isNothing accValM then nonDigitInitAndFurther
                         else completedNumInit ++ nonDigitInitAndFurther
          in  SchematicInfo initList symbolList
      where

        (currVal, xStart, currY) = fromJust accValM
        ch = schematicCharArr UA.! loc
        digVal = ord ch - ord0
        (loc@(x, y) : remainingLocsInRow) = locsRestOfRow

        -- Generate a list of locations and data from the start of the digits to the last digit,
        -- indicated by lastX.

        genScannedValueEntries :: Int -> [(SchematicLoc, SchematicData)]
        genScannedValueEntries lastX
          = [((x', currY), SchematicData (schematicCharArr UA.! (x', currY))
             (Just (currVal, (xStart, currY)))) | x' <- [xStart..lastX]]
