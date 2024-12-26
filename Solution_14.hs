-- For Advent of Code 2023
--
-- By Truman Collins
-- December 2023

module Solution_14 (
  puzzle_14
) where

import Data.List
import Data.Maybe
import Data.Either
import qualified Data.Array.Unboxed as UA
import qualified Data.Array.ST as STA
import qualified Data.Map as M
import Control.Monad
import Control.Monad.ST
import Utilities


data Contents = Empt | CubeRock | RoundRock deriving (Eq, Ord, Show)
data Direction = North | West | South | East deriving (Eq, Show, Enum, Ord, Bounded)

type Location = (Int, Int)
type DishArray = UA.Array Location Contents

-- Constants for this puzzle.

puzzNum :: Int
puzzNum = 14
part2Cycles :: Int
part2Cycles = 1000000000
inputFile :: String
inputFile = "puzzle_14.inp"

--
-- Code for Puzzle 14.
--

puzzle_14 :: IO (Int, Int)
puzzle_14 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInput <- fmap lines (readFile inputFile)

  -- Check for errors in the input 

  let (errorInInput, errorMsg) = checkForInputErrors puzzInput

  when errorInInput
       (ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile errorMsg))

  -- Construct an unboxed array representing the map, and create it with an origin of (1, 1) in the
  -- lower left.
  
  let mapOrigin = (1, 1)
      arrOrError = construct2DUArrayFromInputStrings LowerLeft mapOrigin convSymbol
                   puzzInput

  -- Again, check for errors, this time for map construction problem. These might not ever happen
  -- due to the error checks above.

  when (isLeft arrOrError)
    (let errorCode = fromLeft UnknownError arrOrError
         errorMessage = correspondingArrCreationErrorMessage errorCode
     in  ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile errorMessage))

  -- At this point, with error checks done, get the valid map.

  let mapArr = fromRight undefined arrOrError

  -- Tilt the map North, letting the round rocks roll, then compute the weighted sum for part 1.

  let tiltedMapArr = tiltArray mapArr North
      ansPart1 = computeWeightedSum tiltedMapArr

  -- Now cycle through multiple rounds of tilting all four directions. We look for a cycle where the
  -- map repeats, then once found, figure out where in that cycle the final number falls, then
  -- choose that map and return it. Compute the weighted sum of this map, which will be identical to
  -- the map associated with the final requested number of cycles.

  let (part2MapArr, _) = determineMapEquivToGoalInFirstCycle mapArr
      ansPart2 = computeWeightedSum part2MapArr
      
  return (ansPart1, ansPart2)

-- Figure out the first cycle number the corresponds to the final requested number due to loops in
-- the sequence. Return that map and cycle number.

determineMapEquivToGoalInFirstCycle :: DishArray -> (DishArray, Int)
determineMapEquivToGoalInFirstCycle mapArr = (seqOfMapArrs !! cyclesWithoutLoops, cyclesWithoutLoops)
  where
    cyclesWithoutLoops = part2Cycles - ((part2CyclesAfterFirstInLoop `quot` cycleLen) * cycleLen)
    cycleLen = loopEnd - loopStart
    part2CyclesAfterFirstInLoop = part2Cycles - loopStart
    (loopStart, loopEnd) = lookForLoop 0 M.empty seqOfMapArrs
    seqOfMapArrs = iterate performTiltCycle mapArr

    -- Visit each successive map, inserting it into a Map with its associated cycle count. When we
    -- find one that already exists in the map, we have found a cycle, and return the cycle numbers
    -- of the start and end of it.

    lookForLoop :: Int -> M.Map DishArray Int -> [DishArray] -> (Int, Int)
    lookForLoop _ _ [] = error (genErrStrForErrorFn puzzNum inputFile "Null list in lookForLoop.")
    lookForLoop currCycle currMap (currArr : nextArrs)
      | isNothing foundInd = newCycle `seq` lookForLoop newCycle newMap nextArrs
      | otherwise = (fromJust foundInd, currCycle)
      where
        newCycle = currCycle + 1
        (foundInd, newMap) = M.insertLookupWithKey (\_ x _ -> x) currArr currCycle currMap

-- Perform the tilting process in all four directions, and return the resulting map.

performTiltCycle :: DishArray -> DishArray
performTiltCycle arr = foldl' tiltArray arr [North, West, South, East]

-- Compute the weighted sum of the given map. We can use the y-indices as the weights for round
-- rocks in the map.

computeWeightedSum :: DishArray -> Int
computeWeightedSum = sum . map (snd . fst) . filter ((== RoundRock) . snd) . UA.assocs

tiltArray :: DishArray -> Direction -> DishArray
tiltArray arr dir = runST $ tiltArrayST arr dir dirSpecificDataAndFns
  where
    dirSpecificDataAndFns = case dir of
      North -> ([(x, y) | y <- [yMax, yMax - 1..yMin], x <- [xMin..xMax]],
                (> yMax),
                \(_, y) -> (y, y + 1))
      West  -> ([(x, y) | x <- [xMin..xMax], y <- [yMin..yMax]],
                (< xMin),
                \(x, _) -> (x, x - 1))
      South -> ([(x, y) | y <- [yMin..yMax], x <- [xMin..xMax]],
                (< yMin),
                \(_, y) -> (y, y - 1))
      East  -> ([(x, y) | x <- [xMax, xMax - 1..xMin], y <- [yMin..yMax]],
                (> xMax),
                \(x, _) -> (x, x + 1))
    ((xMin, yMin), (xMax, yMax)) = UA.bounds arr

tiltArrayST :: DishArray -> Direction -> ([Location], Int -> Bool, (Int, Int) -> (Int, Int)) -> ST s DishArray
tiltArrayST initialArr dir (locsToVisit, locOffEdgeFn, nextIndFn) = do
  thawedArr <- STA.thaw initialArr :: ST s (STA.STArray s Location Contents)
  let roundRockLocsToVisit = filter ((== RoundRock) . (initialArr UA.!)) locsToVisit
  mapM_ (rollRock thawedArr) roundRockLocsToVisit
  STA.freeze thawedArr
  where
    rollRock :: STA.STArray s Location Contents -> (Int, Int) -> ST s ()
    rollRock arr (x, y) = do
      let (currInd, nextInd) = nextIndFn (x, y)
      destInd <- findRollEnd arr currInd nextInd
      when (destInd /= currInd) $ do
        STA.writeArray arr (x, y) Empt
        let destLoc = if dir == North || dir == South then (x, destInd) else (destInd, y)
        STA.writeArray arr destLoc RoundRock
      where
        findRollEnd :: STA.STArray s Location Contents -> Int -> Int -> ST s Int
        findRollEnd arr1 currBestInd currInd
          | locOffEdgeFn currInd = return currBestInd
          | otherwise = do
            let currLoc = if dir == North || dir == South then (x, currInd) else (currInd, y)
            currContents <- STA.readArray arr1 currLoc
            if currContents == Empt
              then do
                let nextInd = if dir == North || dir == East then currInd + 1 else currInd - 1
                findRollEnd arr1 currInd nextInd
              else return currBestInd

-- Check for easy-to-spot errors in the input lines.

checkForInputErrors :: [String] -> (Bool, String)
checkForInputErrors inputStrings
  | null inputStrings = (True, "Empty input.")
  | (not . all (== firstLen)) strLengths = (True, "Input string lengths are not equal.")
  | all (\ch -> ch == '.' || ch == '#' || ch == 'O') (concat inputStrings) = (False, "")
  | otherwise = (True, "Unexpected input characters found.")
  where
    strLengths = map length inputStrings
    firstLen   = head strLengths

-- Convert an input character to the map contents enum wrapped in a Maybe.

convSymbol :: Char -> Maybe Contents
convSymbol ch = case ch of
             '.' -> Just Empt
             '#' -> Just CubeRock
             'O' -> Just RoundRock
             _   -> Nothing
