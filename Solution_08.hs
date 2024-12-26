-- For Advent of Code 2023
--
-- By Truman Collins
-- December 2023

module Solution_08 (
  puzzle_08
) where

import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Array as A
import Control.Applicative
import Control.Monad
import Utilities
import Parsers

data LeftRight = LeftF | RightF deriving (Eq, Show)

data NetworkAndInstr = NetworkAndInstr { _leftRightList :: [LeftRight]
                                       , _transArr      :: TransArr
                                       , _nameToIndMap  :: M.Map String Int
                                       , _startIndex    :: Int
                                       , _endIndex      :: Int
                                       } deriving Show

data TransData = TransData { _name         :: String
                           , _leftJumpInd  :: Int
                           , _rightJumpInd :: Int
                           , _endsInA      :: Bool
                           , _endsInZ      :: Bool
                           } deriving Show

type TransArr = A.Array Int TransData
type InitialLineData = (String, (String, String))
type InitialInputData = ([LeftRight], [InitialLineData])

-- I have to say that part two of this problem disappointed me. The way it was written, finding the
-- appropriate cycles could be quite challenging and figuring out how patterns that intersected and
-- joined and how that would all play out was quite a challenge, but the data was set up in a way
-- that there were a number of entirely separate paths and they were quite simple as well so that it
-- was just a matter of applying the least common multiple to the set of cycle lengths. While that
-- was quite easy, it was frustrating because the way it was phrased implied the code should handle
-- much more complicated situations and it feels wrong to just handle the situation as presented
-- because it’s not a general solution. I wish he had either made the description match the
-- simplifying characteristics of the problem, or had made the problem more difficult in the data.

-- Constants for this puzzle.

puzzNum :: Int
puzzNum =  8
inputFile :: String
inputFile = "puzzle_08.inp"

--
-- Code for Puzzle 08.
--

puzzle_08 :: IO (Int, Int)
puzzle_08 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzleInput <- fmap (parse parseInputFile) (readFile inputFile)

  -- Make sure the input was parsed correctly and retrive it out of the returned parser data
  -- structure. If an error is found, a message will be generated and the program will halt.

  inputData <- validateAndReturnParseResult puzzleInput inputFile puzzNum

  -- We know we parsed valid input, but it may have semantic errors. Validate and return a state
  -- array and list of left/right moves.

  networkAndLR <- validateAndCreateArray inputData

  let ansPart1 = countStepsToEnd networkAndLR

  let indsEndingA = indsEndingInA networkAndLR
      cycleLengths = map (getCycleLengths networkAndLR) indsEndingA
      ansPart2 = foldl' lcm 1 cycleLengths

  return (ansPart1, ansPart2)

-- Returns a list of the name indexes that end in 'A'.

indsEndingInA :: NetworkAndInstr -> [Int]
indsEndingInA = map fst . filter (_endsInA . snd) . A.assocs . _transArr

-- Iterate through the network and return a list of each visit to a node with a name ending in
-- 'Z'. Also note the total count, and the count since the last 'Z'. Includes the very first node as
-- well.

zEndSeq :: NetworkAndInstr -> Int -> [(String, Int, Int, Int)]
zEndSeq (NetworkAndInstr lrList transArr _ _ _) startInd
  = (startName, 0, 0, 0) : go 1 startInd 0 lrSeq
  where
    lrSeq = cycle (zip [0..] lrList)
    startName = _name $ transArr A.! startInd

    go :: Int -> Int -> Int -> [(Int, LeftRight)] -> [(String, Int, Int, Int)]
    go _ _ _ [] = error "Null list in zEndSeq."
    go count currInd countSinceLastZ ((lrCount, lrVal) : lrRemaining)
      | endsInZ = nextCount `seq` (currName, count, lrCount, nextCountSinceLastZ)
                  : go nextCount nextInd 0 lrRemaining
      | otherwise = nextCount `seq` go nextCount nextInd nextCountSinceLastZ lrRemaining
      where
        nextCountSinceLastZ = countSinceLastZ + 1
        nextCount = nextCountSinceLastZ `seq` count + 1
        nextInd = if lrVal == LeftF then leftInd else rightInd
        (TransData currName leftInd rightInd _ endsInZ) = transArr A.! currInd

-- Get the cycle length starting at a particular index.

getCycleLengths :: NetworkAndInstr -> Int -> Int
getCycleLengths networkAndInstr ind = cycleLen
  where
    [cycleLen] = (map (\(_, _, _, x) -> x) . take 1 . drop 2) (zEndSeq networkAndInstr ind) 

-- Count the steps to the final node labeled 'ZZZ'.

countStepsToEnd :: NetworkAndInstr -> Int
countStepsToEnd networkInfo = go 0 (_startIndex networkInfo) lrList
  where
    lrList = cycle (_leftRightList networkInfo)
    endIndex = _endIndex networkInfo
    transferArr = _transArr networkInfo

    go :: Int -> Int -> [LeftRight] -> Int
    go _ _ [] = error "Null list in countStopsToEnd."
    go count currInd (currLR : remainLR)
      | currInd == endIndex = count
      | currLR == LeftF = go newCount (_leftJumpInd transData) remainLR
      | otherwise = go newCount (_rightJumpInd transData) remainLR
      where
        transData = newCount `seq` transferArr A.! currInd
        newCount = count + 1

-- Validate the input network, checking for various errors, and return the full data structure
-- representing the network including the array of nodes.

validateAndCreateArray :: InitialInputData -> IO NetworkAndInstr
validateAndCreateArray (lrList, rawTransData) = do

  -- First, sort the data alphabetically based on the initial label, then check to make sure there
  -- aren't any duplicated labels, which if found, is reported as an error.

  let sortedTransData = sortOn fst rawTransData
      groupedLabels = (group . map fst) sortedTransData
      nonUniqueLabels = (not . any (null . tail)) groupedLabels

  when nonUniqueLabels
       (ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile "Non-unique node labels."))

  -- Now create a list of the node labels, and then a map from label to array index. Lookup the
  -- start and end labels, and generate an error if either doesn't exist.
  
  let nodeLabels = map head groupedLabels
      nameToIndMap = M.fromAscList (zip nodeLabels [1..])
      startIndexM = M.lookup "AAA" nameToIndMap
      endIndexM   = M.lookup "ZZZ" nameToIndMap

  when (isNothing startIndexM || isNothing endIndexM)
       (ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile "AAA and ZZZ must exist."))

  -- Now convert all of the named left and right values to their corresponding integers, and make
  -- sure they all convert to something. If they don't, then it is an error.

  let leftAndRightNames = foldr ((\(l, r) acc -> l : r : acc) . snd) [] sortedTransData
      unknownName = any (isNothing . (`M.lookup` nameToIndMap)) leftAndRightNames

  when unknownName
       (ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile "Unknown names in left, right."))

  -- Now put everything together for the datastructure that will hold the graph and some additional
  -- information.

  let sortedIndexedTransData = map convLR sortedTransData
      transArr = A.listArray (1, M.size nameToIndMap) sortedIndexedTransData
      startIndex = fromJust startIndexM
      endIndex = fromJust endIndexM

      -- Take the tuple containing the name of the current node and the left and right nodes that
      -- follow it, and convert that data to a TransData record for the trans array to hold. This
      -- includes the name of the current node, the indices of the left and right nodes, and
      -- booleans indicating whether the current node name ends in an 'A' and 'A'. Note that we can
      -- safely convert the index numbers that are looked up in the node name map because we have
      -- already checked to make sure that all of the left and right nodes are listed as primary
      -- nodes in the input.

      convLR (nm, (lNm, rNm)) = let lastLetter = last nm
                                    endsInA  = endsInZ `seq` 'A' == lastLetter
                                    endsInZ  = leftInd `seq` 'Z' == lastLetter
                                    leftInd  = rightInd `seq` fromJust $ M.lookup lNm nameToIndMap
                                    rightInd = fromJust $ M.lookup rNm nameToIndMap
                                in  endsInA `seq` TransData nm leftInd rightInd endsInA endsInZ
  
  return (NetworkAndInstr lrList transArr nameToIndMap startIndex endIndex)
  
-- Functions to parse the input file.

parseInputFile :: Parser InitialInputData
parseInputFile = do
  leftRightList <- some lOrR
  _ <- space
  nodeTransList <- some parseNodeTransition
  return (leftRightList, nodeTransList)

lOrR :: Parser LeftRight
lOrR = do
    _ <- symbol "L"
    return LeftF
  <|> do
    _ <- symbol "R"
    return RightF

parseNodeTransition :: Parser InitialLineData
parseNodeTransition = do
  nodeName <- identAllAlphaNum
  _ <- space
  _ <- symbol "="
  _ <- space
  _ <- symbol "("
  leftNodeName <- identAllAlphaNum
  _ <- symbol ","
  _ <- space
  rightNodeName <- identAllAlphaNum
  _ <- symbol ")"
  return (nodeName, (leftNodeName, rightNodeName))

-- Parse an identifier beginning with a letter and alpha-numeric after that.

identAllAlphaNum :: Parser String
identAllAlphaNum = do
  x <- alphanum
  xs <- many alphanum
  return (x : xs)
