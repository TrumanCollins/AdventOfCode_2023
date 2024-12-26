-- For Advent of Code 2022
--
-- By Truman Collins
-- December 2, 2023

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
  genErrStrForIOErrorFn,

  -- State Machhine code.

  StateMap,
  StateVec,
  StateAndCount (dfaState_r, dfaCount_r),
  StateSet,
  genStateMap,
  genStateVecFromStateMap,
  genStateVec,
  isAcceptState,
  statesAndCountsStartingAtState0,
  genStatesAndCountsForOneMoreLett,
  sumFinalCountsAfterNSteps,
  genStatesAndCountsForOneSymbol,
  genStatesAndCountsForSymbolList,
  countAcceptStatesFromStateSet,
  countOfAccStatesForSymbolList,
  isAcceptSymbolList,

  subsetsOfSize
) where

import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.Vector as V

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


-- Functions for generating and manipulating state machines.

type StateMap k a = M.Map k (Int, Bool, [(a, [Int])])

-- Generate a state map keyed off of an indicator of the state. The value associated with each key
-- is a triple containing a stateID starting at 0 for the initial state, a boolean indicating
-- whether this is an accept state, and a list of symbols from this point and the next state ID for
-- each.
-- The inputs for this function are: a list of the alphabet symbols, the initial state, a function
-- on a state that indicates true if this is a final state, a function to modify the alphabet to
-- restrict it to those that are valid inputs given the state, and finally, a function that takes a
-- symbol and a state and generates a list of resulting states, possibly empty, after reading the
-- symbol in that state.

genStateMap :: (Ord k, Show k) => [a] -> k -> (k -> Bool) -> (a -> k -> [k])
               -> StateMap k a
genStateMap alphabet initialState isFinalStateFn addSymbolFn
  = let (_, _, fullStateMap) = gsm initialState (0, M.empty)
    in  fullStateMap
  where
    gsm currState (nextAvailID, currMap)
      | isJust mapEntry = (correspondingID, nextAvailID, currMap)
      | otherwise = (nextAvailID, newNextAvailID, newMap)
      where
        mapEntry = M.lookup currState currMap
        (correspondingID, _, _) = fromJust mapEntry

        -- Replace the dummy entry with one with all of the transitions from here. Note that every
        -- state is a final state because we don't create states that aren't because once we have
        -- more than three of the same digit, we can't get back by adding digits.

        newMap = M.insert currState (nextAvailID, isFinalState, stateTransitsFromHere) mapAfterDFS
        isFinalState = isFinalStateFn currState

        -- If we need a new state for the current set of letters, allocate the next free ID for it,
        -- see if it is a final state (all needed letters seen), and create an entry without any
        -- further transitions. After we see what these transitions are and add to the map with the
        -- states reachable from a depth-first search from here, then go back and insert this again
        -- with those transitions. If we don't create this dummy first, then we can get in a loop.

        mapWithDummyKey = M.insert currState (nextAvailID, True, []) currMap
        nextAvailIDAfterThis = nextAvailID + 1

        -- Recursively call this function with each symbol added. Get the new map after this is done
        -- along with all of the transitions from here and the final next available ID.

        (newNextAvailID, mapAfterDFS, stateTransitsFromHere)
          = foldl' recCallForSymbol (nextAvailIDAfterThis, mapWithDummyKey, []) alphabet

        -- Given a symbol add it to the current state and recursively call this function to add it
        -- and its downstream states to the map. Return the new map, new next ID and add the ID
        -- after adding this along with the current symbol to the transition list.

        recCallForSymbol (availID, inMap, accTrans) currSymbol = (newAvailID, nextMap, newSymbolCorrespondences)
          where
            newSymbolCorrespondences = if null corrIDs then accTrans else (currSymbol, corrIDs) : accTrans
            (corrIDs, newAvailID, nextMap) = recurseOnNextStates ([], availID, inMap) statesWithThisSymbol
            statesWithThisSymbol = addSymbolFn currSymbol currState

        -- Recursively call gsm on each of the resulting states from adding this symbol.

        recurseOnNextStates acc [] = acc
        recurseOnNextStates (accIDs, availID, inMap) (thisState : restStates)
          = recurseOnNextStates (corrID : accIDs, newAvailID, nextMap) restStates
          where
            (corrID, newAvailID, nextMap) = gsm thisState (availID, inMap)

---- The input is a state map where the value for each state is a triple (stateID, isFinal,
---- nextStates), where nextStates is a list of pairs (symbol, nextState). The key for the map is
---- ignored by this operation. This function will generate a vector where the index corresponds to
---- the stateID. The elements of the vector are made up of a boolean value indicating if this is a
---- final state or not, and a list of triples containing: (nextStateID, number of symbols going
---- there, and the list of symbols).
--
--type StateVec a = V.Vector (Bool, [(Int, Int, [a])])
--type StateMap k a = M.Map k (Int, Bool, [(a, [Int])])
type StateVec a = V.Vector (Bool, M.Map a [Int])

genStateVecFromStateMap :: (Ord a, Show a) => StateMap k a -> StateVec a
genStateVecFromStateMap stateMap = correspondingStateVec
  where
    correspondingStateVec = V.fromListN (M.size stateMap) vecInitList

    -- Take the elements of the state map, as the key's are no longer needed, and sort them on the
    -- state ID, then map these into pairs with a Bool indicating whether this is a final state or
    -- not, and a collapsed list of transitions. Each of these transitions is a next state and the
    -- number of letters from the usable alphabet at that point that will go to that state.
    
    vecInitList = (map convToCounts . sortBy (compare `on` (\(c, _, _) -> c))
                   . map snd . M.toList) stateMap
    convToCounts (_, isFinal, symbolsAndNextStates) = (isFinal, nextStateMap)
      where
        nextStateMap = (M.fromList . filter (not . null . snd)) symbolsAndNextStates

-- Generate a state vector directly without explicitly generating a state map first. The same steps
-- are taken. This just saves the step of generating the state map first and then passing it to the
-- state vec function.

genStateVec :: (Ord k, Show k, Ord a, Show a) => [a] -> k -> (k -> Bool) -> (a -> k -> [k])
               -> StateVec a
genStateVec alphabet initialState isFinalStateFn addSymbolFn = stateVec
  where
    stateVec = genStateVecFromStateMap stateMap
    stateMap = genStateMap alphabet initialState isFinalStateFn addSymbolFn

-- Holds a state and a count of the number of ways to get to that state.

data StateAndCount = StateAndCount { dfaState_r :: Int
                                   , dfaCount_r :: Integer
                                   } deriving (Show)

-- Return true if the given state and count record represents a final (accept) state.

isAcceptState :: StateVec a -> StateAndCount -> Bool
isAcceptState stateVec (StateAndCount currState _) = fst $ stateVec V.! currState

-- After a number of letters, the total set of states you could be in including the counts of ways
-- of getting to each of these states.

type StateSet = [StateAndCount]

-- Given a state transition vector, begin with a count of 1 at the start state (0), and work through
-- the counts for all possible moves through the state machine. Generate an infinite list of these
-- sets of states and counts, and the caller can do with them what they want.

statesAndCountsStartingAtState0 :: StateVec a -> [StateSet]
statesAndCountsStartingAtState0 transVec
  = iterate (genStatesAndCountsForOneMoreLett transVec) [StateAndCount 0 1]

-- Given the state transition vector and a set of current states and counts, generate the set of
-- states and counts after adding one more letter from the alphabet.

genStatesAndCountsForOneMoreLett :: StateVec a -> StateSet -> StateSet
genStatesAndCountsForOneMoreLett transVec currStatesAndCounts = newSAndC
  where

    -- Generate all states and counts with one additional alphabet letter added from the current
    -- state, then sort and group based on the state, then combine the counts from all identical
    -- states.

    newSAndC = (map combineCounts . groupBy ((==) `on` dfaState_r)
                . sortBy (compare `on` dfaState_r)
                . concatMap oneStepFromState) currStatesAndCounts

    -- Generate the states one step (alphabet letter) away from the given state, multiplying the
    -- current count by the number of alphabet letters resulting in this new state.

    oneStepFromState (StateAndCount st ct) = (map multByCurrCount . filter ((/= 0) . snd)) nextStatesAndCounts
      where
        nextStatesAndCounts = (map (\xs -> (head xs, length xs)) . group . sort . concatMap snd
                              . M.toList . snd) (transVec V.! st)
        multByCurrCount (s, c) = let newCount = fromIntegral c * ct
                                 in  newCount `seq` StateAndCount s newCount

    -- Sum the counts for a set of identical sets.

    combineCounts [] = error "Null list to combineCounts."
    combineCounts stateList = StateAndCount theState combinedCount
      where
        theState = (dfaState_r . head) stateList
        combinedCount = (sum . map dfaCount_r) stateList

-- Begin at the start state, then iterate steps through the state vector accumulating counts at each
-- state after each step. After the given number of steps, sum the counts from the final states.

sumFinalCountsAfterNSteps :: StateVec a -> Int -> Integer
sumFinalCountsAfterNSteps stateVec nth = result
  where

    -- Sum the counts for all of the accept states for the given number of steps.

    result = (sum . map dfaCount_r . filter (isAcceptState stateVec)) statesAndCountsForLenN

    -- Get all of the states and counts after n steps through the state machine.

    statesAndCountsForLenN = statesAndCountsStartingAtState0 stateVec !! nth

---- Given a state and a count (indicating the number of ways we have reached that state from a prior
---- sequence of alphabet symbols), return a list of pairs indicating the next states after consuming
---- the given symbol.

genStatesAndCountsForOneSymbol :: (Ord a, Show a) => StateVec a -> StateSet -> a -> StateSet
genStatesAndCountsForOneSymbol stateVec stateSet symb = newSAndC
  where

    -- Generate all states and counts with one additional alphabet letter added from the current
    -- state, then sort and group based on the state, then combine the counts from all identical
    -- states.

    newSAndC = (map combineCounts . groupBy ((==) `on` dfaState_r)
                . sortBy (compare `on` dfaState_r)
                . concatMap oneStepFromStateWithSymb) stateSet

    -- Generate the states one step (alphabet letter) away from the given state, multiplying the
    -- current count by the number of alphabet letters resulting in this new state.

    oneStepFromStateWithSymb (StateAndCount st ct) = (map multByCurrCount . filter ((/= 0) . snd)) nextStatesAndCounts
      where
        nextStatesAndCounts = (map (\xs -> (head xs, length xs)) . group . sort
                              . getNextStates . snd) (stateVec V.! st)

        getNextStates symbMap
            | isNothing foundNextStates = []
            | otherwise = fromJust foundNextStates
          where
            foundNextStates = M.lookup symb symbMap

        multByCurrCount (s, c) = let newCount = fromIntegral c * ct
                                 in  newCount `seq` StateAndCount s newCount

    -- Sum the counts for a set of identical sets.

    combineCounts [] = error "Null list to combineCounts."
    combineCounts stateList = StateAndCount theState combinedCount
      where
        theState = (dfaState_r . head) stateList
        combinedCount = (sum . map dfaCount_r) stateList

genStatesAndCountsForSymbolList :: (Ord a, Show a) => StateVec a -> [a] -> StateSet
genStatesAndCountsForSymbolList stateVec
  = foldl' (genStatesAndCountsForOneSymbol stateVec) [StateAndCount 0 1]

countAcceptStatesFromStateSet :: (Ord a, Show a) => StateVec a -> StateSet -> Integer
countAcceptStatesFromStateSet stateVec stateSet = result
  where
    result = (sum . map dfaCount_r . filter (isAcceptState stateVec)) stateSet

countOfAccStatesForSymbolList :: (Ord a, Show a) => StateVec a -> [a] -> Integer
countOfAccStatesForSymbolList stVec
  = countAcceptStatesFromStateSet stVec . genStatesAndCountsForSymbolList stVec

isAcceptSymbolList :: (Ord a, Show a) => StateVec a -> [a] -> Bool
isAcceptSymbolList stateVec = (/= 0) . countOfAccStatesForSymbolList stateVec

-- Generate all subsets of length n of a given list.

subsetsOfSize :: Int -> [a] -> [[a]]
subsetsOfSize n xs
  | n < 1 = []
  | otherwise = subsetsOfSize' n (length xs) xs
  where
    subsetsOfSize' :: Int -> Int -> [b] -> [[b]]
    subsetsOfSize' 0 _ _ = [[]]
    subsetsOfSize' _ _ [] = []
    subsetsOfSize' n' len (y : ys)
      | n' > len = []
      | n' == len = [y : ys]
      | otherwise = subsetsOfSize' n' (len-1) ys ++ map (y :) (subsetsOfSize' (n'-1) (len-1) ys)

