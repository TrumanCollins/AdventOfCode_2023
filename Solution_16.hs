-- For Advent of Code 2023
--
-- By Truman Collins
-- December 2023

module Solution_16 (
  puzzle_16
) where

import Data.List
import Data.Either
import qualified Data.Array.Unboxed as UA
import qualified Data.Set as S
import Control.Monad
import Control.Parallel.Strategies
import Utilities


-- Constants for this puzzle.

puzzNum :: Int
puzzNum =  16
inputFile :: String
inputFile = "puzzle_16.inp"

-- Types defined for this puzzle.

data MapElem = Space | LMirror | RMirror | VSplit | HSplit deriving (Eq, Show)
data Direction = North | East | South | West deriving (Eq, Ord, Show)

data BeamState = BeamState { _loc      :: Location
                           , _dir      :: Direction
                           } deriving (Eq, Ord, Show)

type BeamsAndVisited = ([BeamState], BeamLocSet)
type BeamLocSet = S.Set BeamState
type Location = (Int, Int)
type MapArray = UA.Array Location MapElem

--
-- Code for Puzzle 16.
--

-- I solved this in a pretty standard way. I did a breadth-first search from the start location, and
-- also kept a set holding all the nodes visited so far. I then continue the search until my list of
-- current states is empty, and then I know that the set contains all of the location/direction
-- pairs reachable from that starting node. I then take all those locations, get rid of duplicates,
-- and that is the count of nodes reached from that start location.

-- That technique also works for part two, although takes longer. I parallelized the search, but it
-- still does a full search for each starting location.

-- Here is another technique that I think would work well and be faster, but it’s more work and
-- takes more memory. The idea is for each location and direction on the map, have an array or bit
-- vector indicating all of the reachable locations. That could be constructed from the outside in,
-- or even just recursively. The idea is that for each location and direction, generate the next
-- steps from there and take the OR of those locations.

puzzle_16 :: IO (Int, Int)
puzzle_16 = do

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

  -- Here we have the map of cities and empty locations in an unboxed 2-dimensional array.

  let mapArr = fromRight undefined mapArrOrError

  let initialDirAndLoc = (East, (fst mapOrigin, (snd . snd . UA.bounds) mapArr))
      ansPart1 = computeAnsFromInitialStateAndDir mapArr initialDirAndLoc

  let perimeterStartLocations = genPerimeterDirsAndLocs mapArr
--      ansPart2 = (maximum . withStrategy (parList rpar)
      ansPart2 = (maximum . withStrategy (parListChunk 20 rseq)
                  . map (computeAnsFromInitialStateAndDir mapArr)) perimeterStartLocations

  return (ansPart1, ansPart2)

-- Generate all possible start states around the perimeter of the map.

genPerimeterDirsAndLocs :: MapArray -> [(Direction, Location)]
genPerimeterDirsAndLocs mapArr = topRow ++ bottomRow ++ leftCol ++ rightCol
  where
    topRow    = [(South, (x, yMax)) | x <- [xMin..xMax]]
    bottomRow = [(North, (x, yMin)) | x <- [xMin..xMax]]
    leftCol   = [(East, (xMin, y)) | y <- [yMin..yMax]]
    rightCol  = [(West, (xMax, y)) | y <- [yMin..yMax]]
    
    ((xMin, yMin), (xMax, yMax)) = UA.bounds mapArr

-- Given an initial state, compute the number of locations visited by the beam.

computeAnsFromInitialStateAndDir :: MapArray -> (Direction, Location) -> Int
computeAnsFromInitialStateAndDir mapArr (dir, loc) = result
  where
      result = (S.size . S.fromList . map _loc . S.toList . snd . head
                 . dropWhile (not . null . fst) . iterate (stepBeamsUnique mapArr))
               initialState
      initialState = createInitialState dir loc

-- Given a direction and location, generate the initial state and a set containing it.

createInitialState :: Direction -> Location -> BeamsAndVisited
createInitialState dir loc = let initialBeam = BeamState loc dir
                                 initialBeamList = initialBeam `seq` [initialBeam]
                                 initialSet = initialBeamList `seq` S.singleton initialBeam
                             in  initialSet `seq` (initialBeamList, initialSet)

-- Given a set of beam states and a set of already visited beam states, generate the set of beam
-- states one step away, filter this based on the initial set of beam states, then add the current
-- set of states to the beam state set.

stepBeamsUnique :: MapArray -> BeamsAndVisited -> BeamsAndVisited
stepBeamsUnique mapArr (beamStates, visitedSet) = (newBeamStates, newVisitedSet)
  where
    newVisitedSet = foldl' addToSet visitedSet newBeamStates
    newBeamStates = filter notAlreadyVisited (stepBeams mapArr beamStates)

    notAlreadyVisited :: BeamState -> Bool
    notAlreadyVisited beamState = not $ S.member beamState visitedSet

    addToSet :: BeamLocSet -> BeamState -> BeamLocSet
    addToSet beamSet beamState = S.insert beamState beamSet

-- Given a set of beam states, generate the list of states one step from them.

stepBeams :: MapArray -> [BeamState] -> [BeamState]
stepBeams mapArr = concatMap stepBeam
  where

    ((xMin, yMin), (xMax, yMax)) = UA.bounds mapArr

    stepBeam :: BeamState -> [BeamState]
    stepBeam (BeamState currLoc dir) = filter inMapRange nextStates
      where
        mapElem = mapArr UA.! currLoc
        nextStates

          -- Take care of the cases where the beam keeps moving in the same direction. These are
          -- when we are on a space or a splitter where we move through it from the side.

          | mapElem == Space || passThroughSplitter
            = let nextBeamState = stepBeamState dir currLoc
              in  nextBeamState `seq` [nextBeamState]

          -- Here we are sitting on a mirror, so change our direction based on our current direction
          -- and the orientation of the mirror.

          | mapElem == LMirror || mapElem == RMirror
            = let mirrorReflectFn = if mapElem == LMirror then leftMirrorReflect
                                    else rightMirrorReflect
                  reflectDir = mirrorReflectFn dir
                  nextBeamState = stepBeamState reflectDir currLoc
              in  nextBeamState `seq` [nextBeamState]

          -- Here we are hitting asplitter from a direction that will cause a split.

          | otherwise
            = let newDirs = if mapElem == VSplit then [North, South] else [East, West]
              in  map (`stepBeamState` currLoc) newDirs

        -- True if we are sitting on a splitter and our movement direction has us pass through.

        passThroughSplitter = (mapElem == VSplit && (dir == North || dir == South))
                              || (mapElem == HSplit && (dir == East || dir == West))

    -- Given a direction and location, generate a beam state with the location one step in the given
    -- direction.

    stepBeamState :: Direction -> Location -> BeamState
    stepBeamState currDir loc = let newLoc = genNextLoc currDir loc
                                in  newLoc `seq` BeamState newLoc currDir

    -- Given a direction and location, generate the location one step in that direction.

    genNextLoc :: Direction -> Location -> Location
    genNextLoc North (x, y) = let newY = y + 1 in newY `seq` (x, newY)
    genNextLoc West  (x, y) = let newX = x - 1 in newX `seq` (newX, y)
    genNextLoc South (x, y) = let newY = y - 1 in newY `seq` (x, newY)
    genNextLoc East  (x, y) = let newX = x + 1 in newX `seq` (newX, y)

    -- Return the direction of movement given incoming direction for a left-leaning mirror.

    leftMirrorReflect :: Direction -> Direction
    leftMirrorReflect North = West
    leftMirrorReflect East  = South
    leftMirrorReflect South = East
    leftMirrorReflect West  = North

    -- Return the direction of movement given incoming direction for a right-leaning mirror.

    rightMirrorReflect :: Direction -> Direction
    rightMirrorReflect North = East
    rightMirrorReflect East  = North
    rightMirrorReflect South = West
    rightMirrorReflect West  = South

    -- Return true if this beam state is in the range of the map.

    inMapRange :: BeamState -> Bool
    inMapRange (BeamState (x, y) _ )
      | x < xMin || x > xMax = False
      | y < yMin || y > yMax = False
      | otherwise = True
          
-- Convert an input character to the type of map element.

convSymbol :: Char -> Maybe MapElem
convSymbol ch = case ch of
                  '.'  -> Just Space
                  '\\' -> Just LMirror
                  '/'  -> Just RMirror
                  '|'  -> Just VSplit
                  '-'  -> Just HSplit
                  _    -> Nothing

