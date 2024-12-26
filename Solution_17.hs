-- For Advent of Code 2023
--
-- By Truman Collins
-- December 2023

module Solution_17 (
  puzzle_17
) where

import Data.Maybe
import Data.Char
import Data.Either
import qualified Data.Array.Unboxed as UA
import qualified Data.Graph.Inductive.Graph as GR
import qualified Data.Graph.Inductive.PatriciaTree as GRPT
import qualified Data.Graph.Inductive.Query.SP as GRSP
import Control.Monad
--import Debug.Trace
import Utilities


-- Constants for this puzzle.

puzzNum :: Int
puzzNum =  17
inputFile :: String
inputFile = "puzzle_17.inp"

-- Types defined for this puzzle.

data CrucibleState = CrucibleState { _loc       :: MapLoc
                                   , _dir       :: Direction
                                   , _fwdRemain :: Int
                                   } deriving (Show, Eq)

data FullGraph = FullGraph { _startNode :: Int
                           , _destNode  :: Int
                           , _mapGraph  :: MapGraph
                           } deriving Show

data Direction = North | East | South | West deriving (Eq, Show, Enum, Bounded)

type NodeLabel = CrucibleState

type MapLoc = (Int, Int)

type MapArray = UA.Array MapLoc Int

type MapGraph = GRPT.Gr NodeLabel Int

--
-- Code for Puzzle 17.
--

puzzle_17 :: IO (Int, Int)
puzzle_17 = do

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

  -- Here we have the map of city blocks with the numbers representing the amount of heat loss
  -- passing through.

  let mapArr = fromRight undefined mapArrOrError

  let part1MaxAndMinForward = (3, 1)
      ansPart1 = computeMinPathHeatLoss mapArr part1MaxAndMinForward 1

  let part2MaxAndMinForward = (10, 4)
      ansPart2 = computeMinPathHeatLoss mapArr part2MaxAndMinForward 2

  return (ansPart1, ansPart2)

-- Given the map and a pair indicating the maximum and minimum length of a straight run, compute the
-- shortest path (in terms of heat loss) from the start to the finish location, and from that, sum
-- the total heat loss along that path.

computeMinPathHeatLoss :: MapArray -> (Int, Int) -> Int -> Int
computeMinPathHeatLoss mapArr fwdInfo@(maxFwd, _) puzzlePart
  | isNothing maybePath || any isNothing pathInfo
    = let errStrWithPart = "No shortest path for part " ++ show puzzlePart ++ "."
      in  error (genErrStrLocal errStrWithPart)
  | otherwise = pathHeatLoss
  where

    -- Generate a list representing the path with (location, heatLoss) pairs for each step. Then sum
    -- the heat losses for the answer.

    pathHeatLoss = (sum . map (snd . fromJust) . drop 1) pathInfo
    pathInfo = map (nodeNumToCoordAndHeatLoss mapArr mapGraph) path

    -- Get the path with the least cost, and if found, drop the last node in the path, because it is
    -- the special one created for all 16 of the destination locations to be fed into.

    path = (init . fromJust) maybePath
    maybePath = GRSP.sp startNode destNode mapGraph

    -- Generate a graph representing the map, including the special end node.

    (FullGraph startNode destNode mapGraph) = genFullGraphFromArr mapArr startPlace destLoc
                                              specialEndLoc fwdInfo

    -- Create a crucible state that is at the next location outside the map, which will be used in
    -- the graph as the end state. Since the destination corner of the map is really represented by
    -- 16 nodes, each of them will have an edge to this special end node, and there will be no cost
    -- to passing over this edge. Since we can compute the node index from a crucible state, create
    -- this special node to have values that can be used to get the next node index, if it were
    -- needed to be calculated.

    specialEndLoc = (((+ 1) . fst . snd . UA.bounds) mapArr, 0)

    startPlace = CrucibleState (0, (snd . snd . UA.bounds) mapArr) East maxFwd
    destLoc = ((fst . snd . UA.bounds) mapArr, 0)

-- Generate a graph given the map array. Takes in the start crucible state, the destination location
-- (not state, because our direction and remaining forward moves don't matter when reaching the
-- end), and a special 'location' that would be the next location outside of the map (xMax+1,
-- 0). This will be used for the destination node and will have edges from all of the destination
-- location variant nodes. Returned is the resulting graph and the start and destination indices.

genFullGraphFromArr :: MapArray -> CrucibleState -> MapLoc -> MapLoc -> (Int, Int) -> FullGraph
genFullGraphFromArr mapArr startPlace destLoc specialEndLoc fwdInfo@(maxFwd, minFwd)
  | xMin /= 0 || yMin /= 0 = error (genErrStrLocal "Map min X and Y must be 0.")
--  | trace ("Order: " ++ show (GR.order mapGraph)) False = undefined
  | otherwise = FullGraph startNodeIndex destNodeInd mapGraph
  where

    mapGraph = GR.mkGraph nodeList edgeList

    startNodeIndex = compNodeIndex startPlace
    startLoc = _loc startPlace

    -- The dest node index is the index of the special extra node that the nodes corresponding to
    -- the destination location all point to. It has the same index as the number of elements of the
    -- map array, because the indices start at 0.

    destNodeInd  = (xMax - xMin + 1) * (yMax - yMin + 1) * 4 * (maxFwd + 1)

    -- Construct the node list with indices starting at 0 so there are 16 nodes associated with each
    -- location in the map, one for each combination of movement direction and number of straight
    -- moves available from here. This way, we can compute the node index from the crucible state
    -- easily. The final node, added at the end, is the special destination node that a valid path
    -- will end at. One past the destination location, but no additional heat loss.

    nodeList = nodeListFromMap ++ [(destNodeInd, specialEndPlace)]
    nodeListFromMap = zip [0..] allCrucibleStates

    -- Generate all crucible states for all locations on the map. Some of these will be unreachable,
    -- but they will just be ignored.

    allCrucibleStates = [CrucibleState (x, y) (toEnum dirNum) fwdRemain | x <- [xMin..xMax],
                          y <- [yMin..yMax], dirNum <- [0..3], fwdRemain <- [0..maxFwd]]

    -- The list of edges includes all edges representing possible moves in the map, plus the edges
    -- that take us from the 16 nodes representing the destination location to the special final
    -- node.

    edgeList = edgesFromAllMapLocations ++ edgesFromDestToSpecial

    edgesFromAllMapLocations = concatMap (genAllAllowedMoves mapArr fwdInfo startLoc) nodeListFromMap
    edgesFromDestToSpecial = [(compNodeIndex (CrucibleState destLoc dir fwdRemain), destNodeInd, 0)
                               | dir <- [minBound..maxBound], fwdRemain <- [0..(maxFwd - minFwd)]]

    -- We will give the special end node a North direction and 0 for remaining forward moves so that
    -- when we compute the index number from it, we will get one past the largest index number of
    -- the nodes representing map locations.

    specialEndPlace = CrucibleState specialEndLoc North 0

    -- Fill in the first two parameters, which won't change.

    compNodeIndex :: CrucibleState -> Int
    compNodeIndex = computeNodeIndex mapArr maxFwd
    
    ((xMin, yMin), (xMax, yMax)) = UA.bounds mapArr

-- Given a crucible state, compute the corresponding graph index number.

computeNodeIndex :: MapArray -> Int -> CrucibleState -> Int
computeNodeIndex mapArr maxFwdMoves (CrucibleState (x, y) dir fwdRemain) = nodeIndex
  where
    nodeIndex = fwdRemain + fwdMoveOptions * fromEnum dir + dirAndFwdOptionCnt * y
                + (dirAndFwdOptionCnt * (yMax + 1) * x)
    yMax = (snd . snd . UA.bounds) mapArr

    dirAndFwdOptionCnt = dirCount * fwdMoveOptions

    -- We need to keep track of zero to the maximum number of forward moves.

    fwdMoveOptions = maxFwdMoves + 1
    dirCount = 4

-- Given a nodeID from the given graph, return the node label, which in this case is the
-- corresponding array coordinate, along with the heat loss from the map array.

nodeNumToCoordAndHeatLoss :: MapArray -> MapGraph -> Int -> Maybe (MapLoc, Int)
nodeNumToCoordAndHeatLoss mapArr mapGraph nodeInd
  | isNothing maybeNodeLabel = Nothing
  | otherwise = Just (mapLoc, mapArr UA.! mapLoc)
  where
    mapLoc = _loc crucibleState
    crucibleState  = fromJust maybeNodeLabel
    maybeNodeLabel = GR.lab mapGraph nodeInd

genAllAllowedMoves :: MapArray -> (Int, Int) -> MapLoc -> (Int, CrucibleState) -> [(Int, Int, Int)]
genAllAllowedMoves mapArr (maxFwd, minFwd) startLoc (currNodeIndex, CrucibleState loc@(x, y) dir fwdRemain)

  -- The only place we need to have an edge from a node with the maxFwd steps remaining is the start
  -- node.
  
  | fwdRemain == maxFwd && loc /= startLoc = []
  | otherwise = arcFormatMoves
  where
    arcFormatMoves = map convToArcFormat inMapMoves
    inMapMoves = filter inMap firstCutMoves
    firstCutMoves = case dir of
                      North -> if fwdRemain == 0 then eastWestTurns   else northFwd : eastWestTurns
                      East  -> if fwdRemain == 0 then northSouthTurns else eastFwd : northSouthTurns
                      South -> if fwdRemain == 0 then eastWestTurns   else southFwd : eastWestTurns
                      West  -> if fwdRemain == 0 then northSouthTurns else westFwd : northSouthTurns

    -- Only allow turns after the required initial forward movement, except for the initial start
    -- square when a turn can happen because we haven't moved yet.

    (northSouthTurns, eastWestTurns) = if fwdRemain > turnAllowedAt && fwdRemain < maxFwd
                                       then ([], [])
                                       else ([northTurn, southTurn], [eastTurn, westTurn])
    northFwd  = CrucibleState northLoc North fwdOneLess
    northTurn = CrucibleState northLoc North initialFwdLeft
    northLoc  = (x, y + 1)
    eastFwd   = CrucibleState eastLoc East fwdOneLess
    eastTurn  = CrucibleState eastLoc East initialFwdLeft
    eastLoc   = (x + 1, y)
    southFwd  = CrucibleState southLoc South fwdOneLess
    southTurn = CrucibleState southLoc South initialFwdLeft
    southLoc  = (x, y - 1)
    westFwd   = CrucibleState westLoc West fwdOneLess
    westTurn  = CrucibleState westLoc West initialFwdLeft
    westLoc   = (x - 1, y)
    fwdOneLess = fwdRemain - 1

    ((xMin, yMin), (xMax, yMax)) = UA.bounds mapArr

    initialFwdLeft = maxFwd - 1
    turnAllowedAt  = maxFwd - minFwd
    convToArcFormat :: CrucibleState -> (Int, Int, Int)
    convToArcFormat newCState@(CrucibleState (x1, y1) _ _) = (currNodeIndex, newNodeIndex, heatLoss)
      where
        newNodeIndex = computeNodeIndex mapArr maxFwd newCState
        heatLoss = mapArr UA.! (x1, y1)

    -- Check if the location of this state is within the bounds of the map.

    inMap :: CrucibleState -> Bool
    inMap (CrucibleState (x1, y1) _ _)
      | x1 < xMin || x1 > xMax = False
      | y1 < yMin || y1 > yMax = False
      | otherwise = True

-- Convert an input character to the type of map element.

convSymbol :: Char -> Maybe Int
convSymbol ch
  | isDigit ch = Just (ord ch - ord '0')
  | otherwise  = Nothing

-- A fundtion to generate an error string, based on the utility function, but this pre-passes the
-- puzzle number and inputFile name, so the caller only has to give the error string.

genErrStrLocal :: String -> String
genErrStrLocal = genErrStrForErrorFn puzzNum inputFile
