-- For Advent of Code 2023
--
-- By Truman Collins
-- December 2023

module Solution_20 (
  puzzle_20
) where

import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.Array.ST as STA
import qualified Data.Sequence as SQ
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Utilities
import Parsers
import Debug.Trace

data Module = Button String Destinations | Broadcaster String Destinations
              | FlipFlop String LHState Destinations | Output String LHState (Int, Int)
              | Conjunction String LHState InputsAndLevels Destinations Int deriving Show

data QueueAndPulseState = QueueAndPulseState { _pulseCounts :: PulseCounts
                                             , _pulseQueue  :: PulseQueue
                                             } deriving Show

-- When a pulse reaches a module, this data structure is used to indicate the result of that
-- pulse. There may be further pulses to enqueue and we may need to replace the module due to a
-- change in state.

data PulseResult = PulseResult { _modReplacementMaybe :: Maybe Module
                               , _enqueueDestinations :: Bool
                               , _dests               :: Destinations
                               , _outputLH            :: LHState
                               } deriving Show

--data Object = Object1 | Object2 deriving Show

data LHState = Low | High deriving (Eq, Show)
type Destinations = [Destination]
type Destination = (String, Int)
type InputsAndLevels = M.Map Int LHState
type ModuleArray = A.Array Int Module
type ModuleNameMap = M.Map String Int
type PulseCounts = (Int, Int)
type PulseQueue = SQ.Seq QueueElement
type QueueElement = (Int, LHState, Int)
type ModArrAndPulseCounts = (ModuleArray, PulseCounts)


-- Constants for this puzzle.

puzzNum :: Int
puzzNum =  20
inputFile :: String
inputFile = "puzzle_20.inp"

--
-- Code for Puzzle ZZ.
--

puzzle_20 :: IO (Int, Int)
puzzle_20 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzleInput <- fmap (map (parse readInputLine) . lines) (readFile inputFile)

  -- Make sure the input was parsed correctly and retrive it out of the returned parser data
  -- structure. The list of modules returned doesn't yet have index numbers associated with the
  -- modules. If an error is found, a message will be generated and the program will halt.

  moduleListNoIndices <- validateAndReturnParseLinesResult puzzleInput inputFile puzzNum

  when (null moduleListNoIndices)
       (ioError $ userError (genErrStrForIOError "Empty instructions."))

  let (moduleArr, arrIndMap) = createModuleData moduleListNoIndices
      buttonIndexMaybe = M.lookup "button" arrIndMap

  when (isNothing buttonIndexMaybe)
       (ioError $ userError (genErrStrForIOError "No module index for button."))

  let buttonIndex = fromJust buttonIndexMaybe
      (_, (lowPulseCnt, highPulseCnt)) = runST $ processButtonPressesST moduleArr buttonIndex 1000
      ansPart1 = lowPulseCnt * highPulseCnt

  -- The idea here was to run until a single low pulse was seen at the output, but it takes way too
  -- long just running it. There is probably a deeper analysis of what it is doing that is
  -- necessary.

  let ansPart2 = runST $ lookForOutputTrigger moduleArr buttonIndex

  return (ansPart1, ansPart2)

-- Count button presses until output gets a single low pulse.

lookForOutputTrigger :: ModuleArray -> Int -> ST s Int
lookForOutputTrigger moduleArr buttonIndex = do

  modArr <- STA.thaw moduleArr :: ST s (STA.STArray s Int Module)

  pressUntilOneLow modArr 0

  where
    (outputIndex, outputName) = findOutputModuleIndex moduleArr
    buttonDests = getButtonPressDestinations moduleArr buttonIndex

    pressUntilOneLow modArr buttonCntSoFar = do
      let emptyOutput = Output outputName Low (0, 0)
      STA.writeArray modArr outputIndex emptyOutput
      (newModArr, _) <- go modArr buttonIndex buttonDests 1 (QueueAndPulseState (0, 0) SQ.empty)
      outMod <- STA.readArray newModArr outputIndex
      let pulseCntsLH = getLowHighOutputCounts outMod
      if trace (show buttonCntSoFar ++ ": " ++ show pulseCntsLH) (fst pulseCntsLH) == 1 then return buttonCntSoFar
      else do
        let newButtonCount = buttonCntSoFar + 1
        pressUntilOneLow newModArr newButtonCount

findOutputModuleIndex :: ModuleArray -> (Int, String)
findOutputModuleIndex moduleArr
  | null outputIndices = error (genErrStrForError "No output modules detected.")
  | (null . tail) outputIndices = let outputIndAndMod = head outputIndices
                                      outputIndex = fst outputIndAndMod
                                      outputName = (modName . snd) outputIndAndMod
                                  in  (outputIndex, outputName)
  | otherwise = error (genErrStrForError "More than one output module detected.")
  where
    outputIndices = (filter (isOutput . snd) . A.assocs) moduleArr

    isOutput :: Module -> Bool
    isOutput (Output {}) = True
    isOutput _ = False

-- Given the array of modules and the number of times to press the button, this will press the
-- button that number of times and for each press, follow through all of the changes from the pulses
-- moving through the nextwork.

processButtonPressesST :: ModuleArray -> Int -> Int -> ST s ModArrAndPulseCounts
processButtonPressesST moduleArr buttonIndex buttonPushes = do

  -- Thaw the array of modules so we can make changes to it quickly in the ST monad, then call a
  -- function that will push the button the given number of times and follow through with the pulses
  -- for each press as they propagate through the nextwork.
  
  modArr <- STA.thaw moduleArr :: ST s (STA.STArray s Int Module)

  let buttonDests = getButtonPressDestinations moduleArr buttonIndex
  
  (modArrFinished, lowAndHighCounts) <- go modArr buttonIndex buttonDests buttonPushes
                                           (QueueAndPulseState (0, 0) SQ.empty)
  modArrRet <- STA.freeze modArrFinished
  return (modArrRet, lowAndHighCounts)

go :: STA.STArray s Int Module -> Int -> Destinations -> Int -> QueueAndPulseState
      -> ST s (STA.STArray s Int Module, PulseCounts)
go modArr buttonIndex buttonDests buttonPushesLeft (QueueAndPulseState pCnts pulseQueue)

  -- If the queue of pulses is empty, then the series of pulses from the last button press has
  -- completed. We wither have pressed the button all the times we intended to, or we need to
  -- press it again and let the resulting pulses propagate.

  | SQ.null pulseQueue
    = if buttonPushesLeft <= 0 then return (modArr, pCnts)
      else let newBPL = buttonPushesLeft - 1
               newPQ = newBPL `seq` addToPulseQueue buttonIndex buttonDests Low pulseQueue
           in  go modArr buttonIndex buttonDests newBPL (QueueAndPulseState pCnts newPQ)

  -- Here we have one or more pulses in the queue, and we take the last one off the end of the
  -- list and handle it, then call recursively.

  | otherwise = do
      let (remainingPulses SQ.:> currPulse) = SQ.viewr pulseQueue
          currQueueAndPulseState = QueueAndPulseState pCnts remainingPulses
      nextQueueAndPulseState <- handlePulse modArr currQueueAndPulseState currPulse

      -- Recursively handle the next pulse.

      go modArr buttonIndex buttonDests buttonPushesLeft nextQueueAndPulseState

  where

    -- Here we are given a single pulse to a module, and this function will determine whether this
    -- module needs to be replaced because of a change in state and whether to enqueue pulses coming
    -- out of this module.

    handlePulse :: STA.STArray s Int Module -> QueueAndPulseState -> QueueElement
                   -> ST s QueueAndPulseState
    handlePulse modArr' (QueueAndPulseState (lowCnt, highCnt) pulseQueue')
                (srcInd, currLH, destInd) = do

      destMod <- STA.readArray modArr' destInd

      -- Update the pulse counters and see what happens when the destination module receives the
      -- given pulse.

      let newCnts = if currLH == Low then let newLow = lowCnt + 1 in newLow `seq` (newLow, highCnt)
                    else let newHigh = highCnt + 1 in newHigh `seq` (lowCnt, newHigh)
          pulseResult = newCnts `seq` determinePulseOutcome destMod currLH
          modReplacementM = _modReplacementMaybe pulseResult

      -- If something changed about the state of the module, then replace it with the new one in the
      -- module array.

      when (isJust modReplacementM) (STA.writeArray modArr' destInd (fromJust modReplacementM))

      -- If a pulse will propagate out of this module, then enqueue those downstream modules.

      let newPulseQueue
            | _enqueueDestinations pulseResult
              = addToPulseQueue destInd (_dests pulseResult) (_outputLH pulseResult) pulseQueue'
            | otherwise = pulseQueue'

      return (QueueAndPulseState newCnts newPulseQueue)

      where

        -- Given a module and an incoming pulse, determine if the state of the module has changed
        -- along with the new state, and whether downstream pulses should be queued for the
        -- resulting output pulse.

        determinePulseOutcome :: Module -> LHState -> PulseResult
        determinePulseOutcome (Button _ _) _ = PulseResult Nothing False [] Low
        determinePulseOutcome (Broadcaster _ dests) _ = PulseResult Nothing True dests Low
        determinePulseOutcome (FlipFlop _ currLH' _) High = PulseResult Nothing False [] currLH'
        determinePulseOutcome (Output name _ (lCnt, hCnt)) newLH
          = let newLCnt = if newLH == Low then lCnt + 1 else lCnt
                newHCnt = newLCnt `seq` if newLH == High then hCnt + 1 else hCnt
                newModule = newHCnt `seq` Output name newLH (newLCnt, newHCnt)
            in  PulseResult (Just newModule) False [] newLH
        determinePulseOutcome (FlipFlop name currLH' dests) Low
          = let newLH = if currLH' == Low then High else Low
            in  PulseResult (Just (FlipFlop name newLH dests)) True dests newLH
        determinePulseOutcome (Conjunction name currLH' inputsAndLevels dests highCnt') incomingLH
          | isNothing currSrcLHM = error (genErrStrForError $ "Unexpected source for conjunction " ++ show srcInd)
          | incomingLH == currSrcLH = PulseResult Nothing True dests currLH'
          | otherwise = PulseResult (Just newConjModule) True dests newLH
          where
--            outputChange = newLH /= currLH
            newConjModule = Conjunction name newLH newInputsAndLevels dests newHighCnt
            newLH = if newHighCnt == M.size newInputsAndLevels then Low else High
            newHighCnt = if incomingLH == Low then highCnt' - 1 else highCnt' + 1
            newInputsAndLevels = M.insert srcInd incomingLH inputsAndLevels
            currSrcLH  = fromJust currSrcLHM
            currSrcLHM = M.lookup srcInd inputsAndLevels

-- Here we just want to look up the button module, whose index is provided, error check to make sure
-- it is actually the button module, then return the destinations, which should be a list of
-- broadcast modules, although from the definition of the problem, there should be only one of them.

getButtonPressDestinations :: ModuleArray -> Int -> Destinations
getButtonPressDestinations moduleArr buttonIndex = matchButton (moduleArr A.! buttonIndex)
  where
    matchButton :: Module -> Destinations
    matchButton (Button _ dests) = dests
    matchButton _ = error (genErrStrForError "Button modules is not of button type.")

-- Get the low and high pulse counts from an output module.

getLowHighOutputCounts :: Module -> (Int, Int)
getLowHighOutputCounts (Output _ _ cntsLH) = cntsLH
getLowHighOutputCounts _ = error (genErrStrForError "Output modules is not of output type.")

-- Add the given destination indexes to the front of the queue with the given state and source
-- index.

addToPulseQueue :: Int -> Destinations -> LHState -> PulseQueue -> PulseQueue
addToPulseQueue _ [] _ pulseQueue = pulseQueue
addToPulseQueue srcInd ((_, destInd) : remainderDests) lh pulseQueue
  = let newPulseQueue = (srcInd, lh, destInd) SQ.<| pulseQueue
    in  addToPulseQueue srcInd remainderDests lh newPulseQueue
  
createModuleData :: [Module] -> (ModuleArray, ModuleNameMap)
createModuleData moduleList = (moduleArray, moduleNameMap)
  where

    -- Create an array holding all of the modules and with all of them having the corresponding index numbers internally.

    moduleArray = A.array (0, length allModules - 1) (zip [0..] indexedModules)

    -- Now go through all the modules and add the correct index numbers corresponding to the module
    -- names, which involves updating all of the destination lists, and for a conjunction the list
    -- map of inputs as well.

    indexedModules = map (addIndices moduleNameMap modInputMap) allModules

    -- Create a map that for each module index holds a list of module indices that are its inputs.

    modInputMap = foldl' accDestSrcInds M.empty destSrcPairs
    destSrcPairs = concatMap (getDestToSrcList moduleNameMap) moduleListWithButton

    accDestSrcInds :: M.Map Int [Int] -> (Int, Int) -> M.Map Int [Int]
    accDestSrcInds acc (dI, sI) = M.insertWith (\[i] a -> i : a) dI [sI] acc

    -- Generate a list of pairs for the given module of the destination index numbers paired with
    -- the index number of this module as the source.

    getDestToSrcList :: ModuleNameMap -> Module -> [(Int, Int)]
    getDestToSrcList modNmMp modl = zip destInds (repeat srcInd)
      where
        srcInd    = (fromJust . (`M.lookup` modNmMp) . modName) modl
        destInds  = map (fromJust . (`M.lookup` modNmMp)) destNames
        destNames = (map fst . modDestinations) modl

    -- Here we have assigned indexes, starting at 0, to each module name and have put them in a map
    -- of names to index numbers.

    moduleNameMap = M.fromList moduleIndexList
    moduleIndexList = zip (map modName allModules) [0..]

    -- Here we have the list of all the modules, the ones read from the input, the button module,
    -- and any output modules that are named as destinations but not listed in the input.

    allModules = moduleListWithButton ++ outputModules

    -- We generate the list of destinations that are not module names. These will be output modules
    -- that we will add to the list.

    outputModules = (map (\nm -> Output nm Low (0, 0)) . S.toList) outputNames
    outputNames = S.difference destSet srcSet
    destSet = (S.fromList . concatMap snd) srcAndDestNames
    srcSet  = (S.fromList . map fst) srcAndDestNames
    srcAndDestNames = map (\md -> (modName md, (map fst . modDestinations) md)) moduleListWithButton

    -- We add a button module to the list of modules read from the input.

    moduleListWithButton = Button "button" [("broadcaster", 0)] : moduleList

addIndices :: ModuleNameMap -> M.Map Int [Int] -> Module -> Module
addIndices nameMap _ (Button name dests) = Button name (map (addIndToDest nameMap) dests)
addIndices nameMap _ (Broadcaster name dests) = Broadcaster name (map (addIndToDest nameMap) dests)
addIndices nameMap _ (FlipFlop name lhState dests)
  = FlipFlop name lhState (map (addIndToDest nameMap) dests)
addIndices _ _ outp@(Output {}) = outp
addIndices nameMap inputMap (Conjunction name lhState _ dests highCnt)
  = Conjunction name lhState newLHMap newDests highCnt
  where
    newLHMap = M.fromList (zip destInds (repeat Low))
    destInds = fromJust $ M.lookup currInd inputMap
    currInd  = fromJust $ M.lookup name nameMap
    newDests = map (addIndToDest nameMap) dests

addIndToDest :: ModuleNameMap -> Destination -> Destination
addIndToDest nameMap (str, _) = (str, fromJust (M.lookup str nameMap))

modName :: Module -> String
modName (Button name _) = name
modName (Broadcaster name _) = name
modName (FlipFlop name _ _) = name
modName (Output name _ _) = name
modName (Conjunction name _ _ _ _) = name

modDestinations :: Module -> Destinations
modDestinations (Button _ dests) = dests
modDestinations (Broadcaster _ dests) = dests
modDestinations (FlipFlop _ _ dests) = dests
modDestinations (Output {}) = []
modDestinations (Conjunction _ _ _ dests _) = dests

-- Functions to parse the input lines.

readInputLine :: Parser Module
readInputLine = do
    _ <- symbol "broadcaster"
    _ <- space
    _ <- symbol "->"
    _ <- space
    dests <- cslOfIdents
    return (Broadcaster "broadcaster" (zip dests (repeat 0)))
  <|> do
    _ <- symbol "%"
    name <- ident
    _ <- space
    _ <- symbol "->"
    _ <- space
    dests <- cslOfIdents
    return (FlipFlop name Low (zip dests (repeat 0)))
  <|> do
    _ <- symbol "&"
    name <- ident
    _ <- space
    _ <- symbol "->"
    _ <- space
    dests <- cslOfIdents
    return (Conjunction name High M.empty (zip dests (repeat 0)) 0)

-- Generate an error string for the "error" function. This just captures the puzzle number and fine
-- name so we don't have to pass it each time.

genErrStrForError :: String -> String
genErrStrForError = genErrStrForErrorFn puzzNum inputFile

genErrStrForIOError :: String -> String
genErrStrForIOError = genErrStrForIOErrorFn puzzNum inputFile
