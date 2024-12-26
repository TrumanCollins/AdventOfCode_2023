-- For Advent of Code 2022
--
-- By Truman Collins
-- December 2, 2022

module Utilities (
  isqrtG,
  OriginLoc(..),
  ArrayConstrErrors(..),
  PrintRowDirection(..),
  correspondingArrCreationErrorMessage,
  construct2DArrayFromInputStrings,
  construct2DUArrayFromInputStrings,
  print2DUArray,
  print2DArray,
  padFront,
  applyFToBoth,
  applyFToBoth',

  genErrStrForErrorFn,
  genErrStrForIOErrorFn
) where

import Data.Maybe
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA

-- Compute the integral square root of an integral type.

isqrtG :: (Integral a) => a -> a
isqrtG 0 = 0
isqrtG 1 = 1
isqrtG n = head $ dropWhile (\x -> x * x > n)
                $ iterate (\x -> (x + n `quot` x) `quot` 2) (n `quot` 2)

-- The OriginLoc indicates if the array's origin is at the left of the first string (UpperLeft) or
-- the left of the last string (LowerLeft).

data OriginLoc = LowerLeft | UpperLeft deriving Eq
data ArrayConstrErrors = NullInput | NullRow | NonUniformLengths | UnknownCharacter | UnknownError
                         deriving (Eq, Show)
type ArrayIndex  = (Int, Int)
type OriginIndex = ArrayIndex

correspondingArrCreationErrorMessage :: ArrayConstrErrors -> String
correspondingArrCreationErrorMessage NullInput = "Null input string list."
correspondingArrCreationErrorMessage NullRow   = "Null input string."
correspondingArrCreationErrorMessage NonUniformLengths = "Row lengths not all the same."
correspondingArrCreationErrorMessage UnknownCharacter  = "Invalid input character."
correspondingArrCreationErrorMessage UnknownError  = "Unknown error."

-- Construct a 2-D Array.

construct2DArrayFromInputStrings :: OriginLoc -> OriginIndex -> (Char -> Maybe a) -> [String]
                                    -> Either ArrayConstrErrors (A.Array OriginIndex a)
construct2DArrayFromInputStrings originLoc (originX, originY) charConvFn input
  | null input = Left NullInput
  | (null . head) input = Left NullRow
  | any (/= colCount) rowLengths = Left NonUniformLengths
  | (any isNothing . concat) elemsWithMaybe = Left UnknownCharacter
  | otherwise = Right (A.array ((originX, originY), (xMaxInd, yMaxInd)) initList)
  where

    -- Figure out the number of rows and columns.

    rowCount = length input
    colCount = head rowLengths
    rowLengths = map length input

    -- Convert the characters into the wanted elements, but first convert them into Maybe values to
    -- error check. Only if the entire array is good do we convert to the raw element values.

    elemsAfterMaybe = map (map fromJust) elemsWithMaybe
    elemsWithMaybe  = map (map charConvFn) input

    -- Create the list of indices and elements to initialize the array.

    initList = (concatMap genRowInits . zip yRange) elemsAfterMaybe

    genRowInits :: (Int, [a]) -> [(OriginIndex, a)]
    genRowInits (yIndex, elements) = zipWith createInit xRange elements
      where
        createInit :: Int -> a -> (ArrayIndex, a)
        createInit x el = ((x, yIndex), el)

    -- If the origin is on the top left, then the Y-range begins with the origin Y-value, otherwise
    -- the range is reversed. The X-range is simpler.

    yRange = if originLoc == LowerLeft then [yMaxInd,yMaxInd-1..originY] else [originY..yMaxInd]
    xRange = [originX..xMaxInd]
    xMaxInd = originX + colCount - 1
    yMaxInd = originY + rowCount - 1

-- Construct a 2-D Array.

construct2DUArrayFromInputStrings :: OriginLoc -> OriginIndex -> (Char -> Maybe a) -> [String]
                                    -> Either ArrayConstrErrors (UA.Array OriginIndex a)
construct2DUArrayFromInputStrings originLoc (originX, originY) charConvFn input
  | null input = Left NullInput
  | (null . head) input = Left NullRow
  | any (/= colCount) rowLengths = Left NonUniformLengths
  | (any isNothing . concat) elemsWithMaybe = Left UnknownCharacter
  | otherwise = Right (UA.array ((originX, originY), (xMaxInd, yMaxInd)) initList)
  where

    -- Figure out the number of rows and columns.

    rowCount = length input
    colCount = head rowLengths
    rowLengths = map length input

    -- Convert the characters into the wanted elements, but first convert them into Maybe values to
    -- error check. Only if the entire array is good do we convert to the raw element values.

    elemsAfterMaybe = map (map fromJust) elemsWithMaybe
    elemsWithMaybe  = map (map charConvFn) input

    -- Create the list of indices and elements to initialize the array.

    initList = (concatMap genRowInits . zip yRange) elemsAfterMaybe

    genRowInits :: (Int, [a]) -> [(OriginIndex, a)]
    genRowInits (yIndex, elements) = zipWith createInit xRange elements
      where
        createInit :: Int -> a -> (ArrayIndex, a)
        createInit x el = ((x, yIndex), el)

    -- If the origin is on the top left, then the Y-range begins with the origin Y-value, otherwise
    -- the range is reversed. The X-range is simpler.

    yRange = if originLoc == LowerLeft then [yMaxInd,yMaxInd-1..originY] else [originY..yMaxInd]
    xRange = [originX..xMaxInd]
    xMaxInd = originX + colCount - 1
    yMaxInd = originY + rowCount - 1

-- Used to determine whether to pring from the lowest Y-value to the highest (PrintLowYFirst) or to
-- start at the highest Y-value and work to the lowest (PrintHighYFirst).

data PrintRowDirection = PrintLowYFirst | PrintHighYFirst deriving (Eq, Show)

-- Print a boxed array to stdout where the array has is two-dimensional with Int indices and the
-- element has Show defined for it.

print2DArray :: (Show a) => PrintRowDirection -> A.Array (Int, Int) a -> IO ()
print2DArray printYDir tileArr = do
  let ((xLow, yLow), (xHigh, yHigh)) = A.bounds tileArr
      colWidth = ((+ 1) . maximum . map (length . show) . A.elems) tileArr
      yRange = if printYDir == PrintLowYFirst then [yLow..yHigh] else [yHigh,yHigh-1..yLow]
  mapM_ (printRow colWidth xLow xHigh) yRange

  where

    printRow :: Int -> Int -> Int -> Int -> IO ()
    printRow colWidth xLow xHigh y = do
      mapM_ printElem [xLow..xHigh]
      putStrLn ""

      where
        printElem :: Int -> IO ()
        printElem x = putStr $ showAndPadFront colWidth (tileArr UA.! (x, y))

-- Print an unboxed array to stdout where the array has is two-dimensional with Int indices and the
-- element has Show defined for it.

print2DUArray :: (Show a) => PrintRowDirection -> UA.Array (Int, Int) a -> IO ()
print2DUArray printYDir tileArr = do
  let ((xLow, yLow), (xHigh, yHigh)) = UA.bounds tileArr
      colWidth = ((+ 1) . maximum . map (length . show) . UA.elems) tileArr
      yRange = if printYDir == PrintLowYFirst then [yLow..yHigh] else [yHigh,yHigh-1..yLow]
  mapM_ (printRow colWidth xLow xHigh) yRange

  where
    printRow :: Int -> Int -> Int -> Int -> IO ()
    printRow colWidth xLow xHigh y = do
      mapM_ printElem [xLow..xHigh]
      putStrLn ""

      where
        printElem :: Int -> IO ()
        printElem x = putStr $ showAndPadFront colWidth (tileArr UA.! (x, y))

showAndPadFront :: (Show a) => Int -> a -> String
showAndPadFront wantedSize val = let str = show val
                                 in  padFront wantedSize str

padFront :: Int -> String -> String
padFront wantedSize str = replicate (wantedSize - length str) ' ' ++ str

genErrStrForErrorFn :: Int -> String -> String -> String
genErrStrForErrorFn puzzleNum inputFile errorMsg
  = let bodyOfErrorStr = genCommonErrorStr puzzleNum inputFile errorMsg
    in  mconcat ["Error in puzzle ", bodyOfErrorStr]

genErrStrForIOErrorFn :: Int -> String -> String -> String
genErrStrForIOErrorFn puzzleNum inputFile errorMsg
  = let bodyOfErrorStr = genCommonErrorStr puzzleNum inputFile errorMsg
    in  mconcat ["Puzzle ", bodyOfErrorStr]

genCommonErrorStr :: Int -> String -> String -> String
genCommonErrorStr puzzleNum inputFile errorMsg
  = mconcat [show puzzleNum, " with input file \'", inputFile, "\': ", errorMsg]

-- Apply the given function to both elements of a tuple.

applyFToBoth :: (a -> b) -> (a, a) -> (b, b)
applyFToBoth fn (w, v) = (fn w, fn v)

-- The same as above, but strictly.

applyFToBoth' :: (a -> b) -> (a, a) -> (b, b)
applyFToBoth' fn (w, v) = let wf = fn w
                              vf = wf `seq` fn v
                          in  vf `seq` (wf, vf)

