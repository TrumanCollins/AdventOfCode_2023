-- For Advent of Code 2023
--
-- By Truman Collins
-- February 2023

module Solution_09 (
  puzzle_09
) where

import qualified Data.Sequence as SQ
import Control.Monad
import Utilities


-- Constants for this puzzle.

puzzNum :: Int
puzzNum =  9
inputFile :: String
inputFile = "puzzle_09.inp"

--
-- Code for Puzzle 09.
--

puzzle_09 :: IO (Int, Int)
puzzle_09 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInput <- fmap (map (SQ.fromList . map read . words) . lines) (readFile inputFile)

  -- Assuming we get puzzle input from the above line, there really aren't any errors to check.

  let sequencesExtended = map extendSequenceByOne puzzInput
      nonExtendable = filter (not . fst) sequencesExtended

  unless (null nonExtendable)
    (ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile ("Failure to extend: "
                           ++ (show . snd . head) nonExtendable)))

  -- Compute the answers by adding up the last or first elements of the extended sequences.

  let ansPart1 = (sum . map getLast) sequencesExtended
      ansPart2 = (sum . map getFirst) sequencesExtended

  return (ansPart1, ansPart2)

-- Return the last element of the sequence. The sequence is the second element of a tuple.

getLast :: (Bool, SQ.Seq Int) -> Int
getLast (_, sequence') = lastElemOfSequence
  where
    (_ SQ.:> lastElemOfSequence) = SQ.viewr sequence'

-- Return the first element of the sequence. The sequence is the second element of a tuple.

getFirst :: (Bool, SQ.Seq Int) -> Int
getFirst (_, sequence') = firstElemOfSequence
  where
    (firstElemOfSequence SQ.:< _) = SQ.viewl sequence'

-- Find the prior and next number in the series using difference analysis. Return True with the
-- extended sequence if a valid extensions were found, and return False with the original sequence
-- if a valid extensions weren't found.

extendSequenceByOne :: SQ.Seq Int -> (Bool, SQ.Seq Int)
extendSequenceByOne fullSequence

  -- If the sequence is empty, then we were not able to find a stable pattern, so return that there
  -- was an error and the incoming sequence.

  | SQ.null fullSequence = (False, fullSequence)

  -- If we have all zeros, add two more zeros to the sequence and return it.

  | allZeros = (True, 0 SQ.<| (0 SQ.<| fullSequence))

  -- Recursively call this function and then use the first and last elements of the returned
  -- sequence to extend the current one before returning it.

  | otherwise = let (goodBelow, retSeq) = extendSequenceByOne zippedSequence
                    (firstRetSeq SQ.:< _) = SQ.viewl retSeq
                    (_ SQ.:> lastRetSeq) = SQ.viewr retSeq
                    newFullSequence = (firstSeq - firstRetSeq)
                                        SQ.<| (fullSequence SQ.|> (lastSeq + lastRetSeq))
                in  if not goodBelow then (False, fullSequence)
                    else (True, newFullSequence)
  where
    allZeros = foldr checkForNotZero True fullSequence
    zippedSequence = SQ.zipWith (-) tailSeq fullSequence
    (firstSeq SQ.:< tailSeq) = SQ.viewl fullSequence
    (_ SQ.:> lastSeq) = SQ.viewr fullSequence
    checkForNotZero :: Int -> Bool -> Bool
    checkForNotZero x acc
      | x /= 0 = False
      | otherwise = acc
  
