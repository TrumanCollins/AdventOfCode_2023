-- For Advent of Code 2023
--
-- By Truman Collins
-- December 2023

module Solution_05 (
  puzzle_05
) where

import Data.List
import Control.Applicative
import Parsers
import Utilities

data Categories = Seed | Soil | Fertilizer | Water | Light | Temperature | Humidity | Location
                  deriving (Show, Eq)

type TransList = [ConvRanges]

data InputData = InputData { _seeds        :: [Int]
                           , _seedsPart2   :: [IndexRange]
                           , _seedToSoil   :: TransList
                           , _soilToFert   :: TransList
                           , _fertToWater  :: TransList
                           , _waterToLight :: TransList
                           , _lightToTemp  :: TransList
                           , _tempToHum    :: TransList
                           , _humToLoc     :: TransList
                           } deriving Show

type ConvRanges = (IndexRange, IndexRange)
type IndexRange = (Int, Int)
type PossibleLocRanges = [IndexRange]

-- Constants for this puzzle.

puzzNum :: Int
puzzNum =  5
inputFile :: String
inputFile = "puzzle_05.inp"

--
-- Code for Puzzle 05.
--

puzzle_05 :: IO (Int, Int)
puzzle_05 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInput <- fmap (parse parseInputFile) (readFile inputFile)

  -- Get the parsed result, and make sure there wasn't an error.

  resultFromParse <- validateAndReturnParseResult puzzInput inputFile puzzNum

  let rangeConversionPipeline = genRangeConversionPipeline resultFromParse

  -- Compute the answer to part 1. Note that I initially coded this to handle individual seed
  -- locations through the pipeline, but after doing part 2, I converted part 1 to use the same code
  -- and converted a single seed location to a range of size 1 at that location.

  let seedLocsAsUnitRanges = map (\x -> (x, x)) (_seeds resultFromParse)
      finalSeedRangesP1 = map (findFinalSeedRanges rangeConversionPipeline) seedLocsAsUnitRanges
      ansPart1 = findMinimumLoc finalSeedRangesP1

  -- Answer part 2. It's kind of cool how an initial range gets split into pieces and spread all
  -- over the place as it goes through the conversion pipeline. It would be interesting to see
  -- graphically what is happening.

  let finalSeedRangesP2 = map (findFinalSeedRanges rangeConversionPipeline) (_seedsPart2 resultFromParse)
      ansPart2 = findMinimumLoc finalSeedRangesP2

  return (ansPart1, ansPart2)

-- Given a list of resulting possible locations for each seed, find the minimum value for each seed,
-- and then the minimum value among all the seeds for the result.  Note that the location reanges
-- for each seed are sorted, so the first range in the list has the smallest location.

findMinimumLoc :: [PossibleLocRanges] -> Int
findMinimumLoc = minimum . map (fst . head)

-- Take an initial range and run it through the transition pipeline resulting in a list of ranges
-- representing all of the loations an initial location in the starting range could end up.

findFinalSeedRanges :: [TransList] -> IndexRange -> PossibleLocRanges
findFinalSeedRanges rangePipeline initialSeedRange = resultRanges
  where
    resultRanges = foldl' processOneTrans [initialSeedRange] rangePipeline

    -- Move one step through the pipeline. We start with a list of ranges, push each of those ranges
    -- through the transition changes, resulting in a list of ranges, which we merge with the
    -- results of the other incoming ranges.

    processOneTrans :: PossibleLocRanges -> TransList -> PossibleLocRanges
    processOneTrans inRanges rangeTransitions = mergeAndSort outRangesUnmerged
      where
        outRangesUnmerged = concatMap (processOneRange rangeTransitions) inRanges

    -- Take a list of ranges, sort them, and then merge as to minimize the number of ranges.

    mergeAndSort :: PossibleLocRanges -> PossibleLocRanges
    mergeAndSort = mergeRanges . sortOn fst
      where

        -- There are really only three interesting cases to consider. First, if there are no more
        -- ranges, then we are done, and if there is only one range, we are done also. If we have at
        -- least two, then if the first is entirely separate from the second, leave it alone in the
        -- output and move on with the second range and any further ranges. If the first range
        -- entirely overlaps the second, then toss the second one, and move on with the first and
        -- any remaining ranges. If the two ranges overlap, then merge them into one, and move on
        -- with that range an any remaining ranges.

        mergeRanges :: PossibleLocRanges -> PossibleLocRanges
        mergeRanges [] = []
        mergeRanges [x] = [x]
        mergeRanges (x@(xLow, xHigh) : y@(yLow, yHigh) : remaining)
          | yLow > xHigh = x : mergeRanges (y : remaining)
          | xHigh >= yHigh = x : mergeRanges remaining
          | otherwise = (xLow, yHigh) : mergeRanges remaining

    -- We have a single range passing through one of the transitions stages that is defined by a a
    -- sorted list of ranges, and how ranges intersecting that range are shiften on the way out. We
    -- work our way through this list of ranges transitioning the whole input range or pieces of it
    -- until we have gone past the end of that range or have transitioned the whole thing. Several
    -- cases here to consider.

    processOneRange :: TransList -> IndexRange -> PossibleLocRanges
    processOneRange [] range = [range]
    processOneRange (((sLow, sHigh), (dLow, dHigh)) : tRem) range@(low, high)

      -- The range doesn't is past the current transition range, so move on to the next transition.

      | low > sHigh = processOneRange tRem range

      -- The range is below the current transition range, and since they are in order, we can just
      -- return this range, as it won't be affected by any other higher ones.

      | high < sLow = [range]

      -- Here, part of the range is before the transition range, and either the rest is in, or part
      -- is in and part is on the other side. In the first case, we don't have to look further, but
      -- in the second we do.

      | low  < sLow = if high <= sHigh then
                        [(low, sLow - 1), (dLow, dLow + (high - sLow))]
                      else (low, sLow - 1) : (dLow, dHigh) : processOneRange tRem (sHigh + 1, high)

      -- This is the case where the start of the range is within the transition range.

      | otherwise = let transAdj = dLow - sLow
                    in  if high <= sHigh then [(low + transAdj, high + transAdj)]
                        else (low + transAdj, dHigh) : processOneRange tRem (sHigh + 1, high)
    
genRangeConversionPipeline :: InputData -> [TransList]
genRangeConversionPipeline inputData = map (\fn -> fn inputData) convFns
  where
    convFns = [_seedToSoil, _soilToFert, _fertToWater, _waterToLight, _lightToTemp, _tempToHum,
                _humToLoc]

-- Functions to parse the input file. This one here is the top level one that parses the whole file.

parseInputFile :: Parser InputData
parseInputFile = do
  _ <- symbol "seeds:"
  seedInds <- spaceSepInts
  _ <- space
  _ <- symbol "seed-to-soil map:"
  seedToSoil <- listOfRanges
  _ <- space
  _ <- symbol "soil-to-fertilizer map:"
  soilToFert <- listOfRanges
  _ <- space
  _ <- symbol "fertilizer-to-water map:"
  fertToWater <- listOfRanges
  _ <- space
  _ <- symbol "water-to-light map:"
  waterToLight <- listOfRanges
  _ <- space
  _ <- symbol "light-to-temperature map:"
  lightToTemp <- listOfRanges
  _ <- space
  _ <- symbol "temperature-to-humidity map:"
  tempToHum <- listOfRanges
  _ <- space
  _ <- symbol "humidity-to-location map:"
  humToLoc <- listOfRanges
  let seedsPart2 = interpretSeedsAsRanges seedInds
  return (InputData seedInds seedsPart2 seedToSoil soilToFert fertToWater waterToLight lightToTemp
          tempToHum humToLoc)

-- Convert the range conversions for the various transitions into a list of them in order to be used
-- as a pipeline.

interpretSeedsAsRanges :: [Int] -> [(Int, Int)]
interpretSeedsAsRanges [] = []
interpretSeedsAsRanges [_]
  = error (genErrStrForErrorFn puzzNum inputFile "Must have an even number of seed locations.")
interpretSeedsAsRanges (x : y : xs) = (x, x + y) : interpretSeedsAsRanges xs

-- Read an un-empty series of transition ranges.

listOfRanges :: Parser TransList
listOfRanges = do
  ranges <- some rangeReadThree
  let sortedRanges = sortOn (fst . fst) ranges
  return sortedRanges
  where
    rangeReadThree :: Parser ConvRanges
    rangeReadThree = do
      _ <- space
      destStart <- nat
      _ <- space
      srcStart <- nat
      _ <- space
      rangeSize <- nat
      let srcAndDestRanges = buildRangeMap srcStart destStart rangeSize
      return srcAndDestRanges

    buildRangeMap :: Int -> Int -> Int -> ConvRanges
    buildRangeMap srcStart destStart rangeSize
      = ((srcStart, srcStart + rangeSize - 1), (destStart, destStart + rangeSize - 1))

-- Read an un-empty series of natural numbers separated by spaces.

spaceSepInts :: Parser [Int]
spaceSepInts = do
  n <- spaceNat
  ns <- many spaceNat
  return (n : ns)
  where
    spaceNat :: Parser Int
    spaceNat = do
      _ <- space
      nat
