-- For Advent of Code 2023
--
-- By Truman Collins
-- December 2023

module Solution_12 (
  puzzle_12
) where

import Control.Applicative
import Parsers
import Utilities

data Spring = GoodSpring | BadSpring | UnkSpring deriving (Eq, Ord, Show)

data SpringBlock = Damaged Int | DamagedActive Int | Good deriving (Eq, Ord, Show)

type DamagedBlocks = [SpringBlock]

type SpringRow = [Spring]
type Entry = (SpringRow, [Int])

-- Constants for this puzzle.

puzzNum :: Int
puzzNum =  12
inputFile :: String
inputFile = "puzzle_12.inp"

--
-- Code for Puzzle 12.
--

-- This was a fun problem but part two made it clear that my solution in part one would not
-- scale. Initially I did a depth-first search for all possible matches for these patterns, and that
-- worked great for part one. Then when I tried part two it got through many of them slowly but got
-- stuck on others and was taking a very long time. I realized I could use a state machine to
-- represent the different clumps of springs that were broken, and made a generic state machine
-- generator, that I am pleased with.

puzzle_12 :: IO (Int, Int)
puzzle_12 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzleInput <- fmap (map (parse readInputLine) . lines) (readFile inputFile)

  -- Make sure the input was parsed correctly and retrive it out of the returned parser data
  -- structure. If an error is found, a message will be generated and the program will halt.

  springEntries <- validateAndReturnParseLinesResult puzzleInput inputFile puzzNum

  let ansPart1 = (sum . map determineValidPlacementCount) springEntries

  let unfoldedSpringEntries = map unfoldEntry springEntries
      ansPart2 = (sum . map determineValidPlacementCount) unfoldedSpringEntries

  return (ansPart1, ansPart2)

-- Expand the entry for part 2.

unfoldEntry :: Entry -> Entry
unfoldEntry (springRow, counts) = (unfoldedRow, unfoldedCounts)
  where
    unfoldedRow = (tail . concat . replicate 5 . (UnkSpring :)) springRow
    unfoldedCounts = (concat . replicate 5) counts

-- Count the number of valid placements of bad springs for the given row description and list of
-- clumps. This is done using a state machine.

determineValidPlacementCount :: Entry -> Int
determineValidPlacementCount (inRow, clumps) = result
  where
    result = fromIntegral $ countOfAccStatesForSymbolList stateVec inRow
    stateVec = genStateVec [GoodSpring, BadSpring, UnkSpring] stateMachInitialState
      isAccept addSymbolFn
    stateMachInitialState = createInitialState clumps

    -- The state is a list of items and spaces built from the list of clumps. There is a good spring
    -- that must be matched between bad spring groups, a bad spring group that we haven't started
    -- yet, and one that we are in the middle of.

    createInitialState :: [Int] -> DamagedBlocks
    createInitialState [] = []
    createInitialState [x] = [Damaged x]
    createInitialState (x : xs) = Damaged x : Good : createInitialState xs

    -- We are in an accept state if we have matched all of the bad spring clumps.

    isAccept :: DamagedBlocks -> Bool
    isAccept [] = True
    isAccept _ = False

    -- Given a current symbol and a state, generate the zero, one, or two states that result from
    -- consuming the symbol. Two are possible if are ready to start a bad spring clump and we see a
    -- spring in unknown state. Then we consider it either matched as a bad spring or as a good one,
    -- each leading to different states.

    addSymbolFn :: Spring -> DamagedBlocks -> [DamagedBlocks]
    addSymbolFn GoodSpring [] = [[]]
    addSymbolFn BadSpring  [] = []
    addSymbolFn UnkSpring  [] = [[]]
    addSymbolFn GoodSpring (Good : xs) = [xs]
    addSymbolFn BadSpring  (Good : _) = []
    addSymbolFn UnkSpring  (Good : xs) = [xs]
    addSymbolFn GoodSpring dbList@(Damaged _ : _) = [dbList]
    addSymbolFn BadSpring  (Damaged n : xs)
      | n == 1 = [xs]
      | otherwise = [DamagedActive (n - 1) : xs]
    addSymbolFn UnkSpring  dbList@(Damaged n : xs)
      | n == 1 = [dbList, xs]
      | otherwise = [dbList, DamagedActive (n - 1) : xs]
    addSymbolFn GoodSpring (DamagedActive _ : _) = []
    addSymbolFn BadSpring  (DamagedActive n : xs)
      | n == 1 = [xs]
      | otherwise = [DamagedActive (n - 1) : xs]
    addSymbolFn UnkSpring  (DamagedActive n : xs)
      | n == 1 = [xs]
      | otherwise = [DamagedActive (n - 1) : xs]

-- Functions to parse the input lines.

readInputLine :: Parser Entry
readInputLine = do
  springStates <- some readSpring
  _ <- space
  clumps <- cslOfNats
  return (springStates, clumps)

readSpring :: Parser Spring
readSpring = do
    _ <- symbol "."
    return GoodSpring
  <|> do
    _ <- symbol "#"
    return BadSpring
  <|> do
    _ <- symbol "?"
    return UnkSpring
