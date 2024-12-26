-- For Advent of Code 2023
--
-- By Truman Collins
-- December 2023

module Solution_10 (
  puzzle_10
) where

import Data.List
import Data.Maybe
import Data.Either
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.Array.ST as STA
import Control.Monad
import Control.Monad.ST
import Utilities


-- Constants for this puzzle.

puzzNum :: Int
puzzNum =  10
inputFile :: String
inputFile = "puzzle_10.inp"

-- Types defined for this puzzle.

data MazeElem = Ground | Start | Vertical | Horizontal | LLBend | LRBend | URBend | ULBend
                deriving (Eq, Show)

data MazePathInOut = PathElem | LeftOfPath | RightOfPath | Inside | Outside | Undetermined
                deriving (Eq, Enum, Bounded)
instance Show MazePathInOut where
  show PathElem     = "PE"
  show LeftOfPath   = "Lf"
  show RightOfPath  = "Rt"
  show Inside       = "In"
  show Outside      = "Ou"
  show Undetermined = "Un"

data Direction = North | East | South | West | Unknown deriving (Eq, Ord, Show)

data Path = Path { _currLoc   :: Location
                 , _currDir   :: Direction
                 , _priorLocs :: [LocDirAndSteps]
                 } deriving Show

type Location = (Int, Int)
type LocDirAndSteps = (Location, Direction, Int)
type LocAndDir = (Location, Direction)
type MazeArray = UA.Array Location MazeElem
type StepArray = UA.Array Location Int
type InsideOutsideArray = A.Array Location MazePathInOut


--
-- Code for Puzzle 10.
--

puzzle_10 :: IO (Int, Int)
puzzle_10 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInput <- fmap lines (readFile inputFile)

  -- Read in the maze array, converting to an enum for each element and error checking as well.

  let mazeOrigin = (0, 0)
      mazeArrOrError = construct2DUArrayFromInputStrings LowerLeft mazeOrigin convSymbol puzzInput

  -- Check for errors in the input.

  when (isLeft mazeArrOrError)
    (let errorCode = fromLeft UnknownError mazeArrOrError
         errorMessage = correspondingArrCreationErrorMessage errorCode
     in  ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile errorMessage))

  -- Convert the character array that was read in into a more informative array that has more useful
  -- data about the numbers. Also harvest a list of all of the symbols that are not periods.

  let mazeArrWithStart = fromRight undefined mazeArrOrError
      startLocOrError = findStartLoc mazeArrWithStart

  when (isLeft startLocOrError)
    (let errorMessage = fromLeft "Unknown error." startLocOrError
     in  ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile errorMessage))

  -- Try to figure out what tile is under the Start symbol. We look at the adjacent locations to see
  -- what pipes lead to the start location, and if there are two, then we choose the shape that
  -- connects to both.

  let startLoc = fromRight undefined startLocOrError
      mazeArrM = deduceElemUnderStart mazeArrWithStart startLoc

  when (isNothing mazeArrM)
    (ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile "Can't deduce start pipe."))

  -- In this problem, it is specified that there is one loop path, and we should find two paths, one
  -- transiting the loop in each direction.

  let mazeArr = fromJust mazeArrM
      paths = bfsPaths mazeArr startLoc (Path startLoc Unknown [])

  when (length paths /= 2)
    (ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile "There must be exactly 2 paths."))

  -- Compute the answer to part 1 by stepping through the paths, counting the number of steps. We
  -- then generate an array where the value associated with each location is the minimum number of
  -- steps to each location. Locations not on a path hold maxBound, which is filtered out later when
  -- finding the maximum distance along a path.

  let locsAndSteps = (map (\(loc, _, steps) -> (loc, steps)) . concat) paths
      arrBounds = UA.bounds mazeArr
      mazeMinSteps :: StepArray
      mazeMinSteps = UA.accumArray min maxBound arrBounds locsAndSteps
      ansPart1 = (maximum . filter (/= maxBound) . UA.elems) mazeMinSteps

  -- Generate an array indicating which elements are on the path, inside the path, and outside of
  -- it, then count the inside ones.

  let mazeInsideAndOutside = determineInsideAndOutsideOfPath mazeArr (head paths)

  let ansPart2 = (length . filter (== Inside) . A.elems) mazeInsideAndOutside

  return (ansPart1, ansPart2)

-- Generate a new array corresponding to the maze array that labels locations as Inside, Outside, or
-- PathElements.

determineInsideAndOutsideOfPath :: MazeArray -> [LocDirAndSteps] -> InsideOutsideArray
determineInsideAndOutsideOfPath mazeArr path = finalInOutArr
  where
    finalInOutArr = UA.array mazeBounds
                             ((map (convToInOut leftToRightTo) . UA.assocs) initialLeftRightArr)
    leftToRightTo = if outsideEnum == LeftOfPath then (Outside, Inside) else (Inside, Outside)

    -- Try looking at the border elements to see if one of them has been labeled as to the left or
    -- right of the path.  If we didn't find a non-path element along the perimeter, then the entire
    -- permeter must be path. In that case, look for a left or right element anywhere in the maze,
    -- which will indicate the inside. If we don't find one of these, then the path itself takes up
    -- the whole maze, and then it doesn't matter because the count of both inside and outside will
    -- be zero.

    outsideEnum
      | outsideEnumFirstCut /= Undetermined = outsideEnumFirstCut
      | otherwise = let innerLRs = (filter (\x -> x == LeftOfPath || x == RightOfPath)
                                    . UA.elems) initialLeftRightArr
                    in  if null innerLRs then LeftOfPath else head innerLRs
    outsideEnumFirstCut = foldr findLeftOrRight Undetermined borderLocations
    borderLocations = concat ([[(x, yLow), (x, yHigh)] | x <- [xLow..xHigh]]
                              ++ [[(xLow, y), (xHigh, y)] | y <- [yLow..yHigh]])

    -- Work along the path in a particular direction (it doesn't matter which) to mark elements on
    -- one side as to the left and the other as to the right. We won't know hich is inside or
    -- outside initially, but once all these are marked, we can look at the boarders of the maze to
    -- determine whether left or right is inside.

    initialLeftRightArr = runST genInOutArr
    pathLocs = map (\(loc, _, _) -> loc) path
    mazeBounds@((xLow, yLow), (xHigh, yHigh)) = UA.bounds mazeArr

    -- Used by a foldr to find the first element marked left or right of the path on the boarder
    -- locations. Once we have found the first we can just return it and not the accumulator because
    -- we only care about the first one we find.

    findLeftOrRight :: Location -> MazePathInOut -> MazePathInOut
    findLeftOrRight loc acc
      | currVal == LeftOfPath || currVal == RightOfPath = currVal
      | otherwise = acc
      where
        currVal = initialLeftRightArr UA.! loc

    -- Used to convert a single element at a location from left/right to inside/outside based on the
    -- mapping passed in.

    convToInOut :: (MazePathInOut, MazePathInOut) -> (Location, MazePathInOut) -> (Location, MazePathInOut)
    convToInOut (leftTo, rightTo) (loc, leftOrRight)
      | leftOrRight == LeftOfPath  = (loc, leftTo)
      | leftOrRight == RightOfPath = (loc, rightTo)
      | otherwise = (loc, leftOrRight)

    -- This needs to run in the ST monad so that the array we are building can be modifed as we walk
    -- down the path and mark locations reachable to the left and right of the path.

    genInOutArr :: ST s InsideOutsideArray
    genInOutArr = do
      let bnds = UA.bounds mazeArr
      inOutArray <- STA.newArray bnds Undetermined :: ST s (STA.STArray s Location MazePathInOut)

      -- Write the path locations to the array, which won't be inside or outside the path.

      mapM_ (\loc -> STA.writeArray inOutArray loc PathElem) pathLocs

      -- Now do a depth-first search from each path location on the left and right sides, marking
      -- all reachable locations appropriately.

      mapM_ (markLeftAndRight inOutArray) path

      STA.freeze inOutArray

      where

        -- Given the array and the current location and facing direction, use a depth-first search
        -- to mark all the non-path locations to the left and right (and those reachable through
        -- horizontal and vertical steps) as to the left or right.
        -- Note that I use inOutArray' in this function and inOutArray'' in setAllReachable. I do
        -- this because I got a message at runtime that inOutArr was shadowning another
        -- object. After trying several renamings where I still got that message, I tried this, but
        -- still had to do a full rebuild for it to go away. I'm not sure what the issue was or why
        -- I had to do a full rebuild to get it to go away.

        markLeftAndRight :: STA.STArray s Location MazePathInOut -> LocDirAndSteps -> ST s ()
        markLeftAndRight inOutArray' (loc, dir, _) = do
          let (leftLocs, rightLocs) = findLocsToLeftAndRight
          unless (null leftLocs)  (mapM_ (setAllReachable inOutArray' LeftOfPath) leftLocs)
          unless (null rightLocs) (mapM_ (setAllReachable inOutArray' RightOfPath) rightLocs)
          where

            -- Return lists of the locations to the left and right of the current location, given
            -- the type of pipe. Horizontal and vertical pipes have one to the left and one to the
            -- right, assuming the direction corresponds to the horizontal or vertical. In the case
            -- of corner pipes, we will have two locations for one of the directions and none for
            -- the other.

            findLocsToLeftAndRight :: ([Location], [Location])
            findLocsToLeftAndRight = clippedResult
              where
                clippedResult = applyFToBoth (filter (inMazeRange mazeArr)) (leftLocs, rightLocs)
                (leftLocs, rightLocs) = determineLeftAndRightLocs loc dir (mazeArr UA.! loc)

            -- Set all of the Undetermined elements reachable from here via single vertical or
            -- horizontal steps, to the given value. If the current spot doesn't have the value
            -- of Undetermined, then we don't have to do anything.

            setAllReachable :: STA.STArray s Location MazePathInOut -> MazePathInOut -> Location
                               -> ST s ()
            setAllReachable inOutArray'' valToSet loc' = do
              currVal <- STA.readArray inOutArray'' loc'

              when (currVal == Undetermined) $ do
                STA.writeArray inOutArray'' loc' valToSet
                let validNextToLocs = adjacentValidLocations mazeArr loc'
                mapM_ (setAllReachable inOutArray'' valToSet) validNextToLocs

-- Given the location of the star, deduce the pipe kind there from those around it, assuming that
-- there will be two that connect to it.

deduceElemUnderStart :: MazeArray -> Location -> Maybe MazeArray
deduceElemUnderStart mazeArrWithStart startLoc
  | length directions /= 2 = Nothing
  | otherwise = let startPipe = case directions of
                                  [North, East]  -> LLBend
                                  [North, South] -> Vertical
                                  [North, West]  -> LRBend
                                  [East, South]  -> ULBend
                                  [East, West]   -> Horizontal
                                  [South, West]  -> URBend
                                  _              -> error (genErrStrForErrorFn puzzNum inputFile
                                                    "Unepected direction pair deducing start pipe.")
                in  Just (mazeArrWithStart UA.// [(startLoc, startPipe)])
  where
    directions = (sort . map snd) adjacentWithConnsBack
    adjacentWithConnsBack = filter hasConnBack possConns
    possConns = adjacentPossibleConnectionsWithDir mazeArrWithStart startLoc

    hasConnBack :: LocAndDir -> Bool
    hasConnBack (loc, _) = startLoc `elem` possBackConns
      where
        possBackConns = adjacentPossibleConnections mazeArrWithStart loc

-- Do a breadth-first search from the start location along all paths until each either ends or
-- returns to the start lcoation. With the input data, it appears that that there are no false
-- paths, only the one loop that we follow as two paths, one going each direction.

bfsPaths :: MazeArray -> Location -> Path -> [[LocDirAndSteps]]
bfsPaths mazeArray startLoc initialPath = go 0 [initialPath]
  where

    -- The recursion can leave out the maze array and start location, since they will be
    -- unchanged. Take all of the current paths and update them to step one step in any possible
    -- direction that doesn't take them back to their prior location. Take these and remove any that
    -- have come back to the starting location, and return their paths and counts before recursively
    -- calling for those paths that haven't come back to the start.

    go :: Int -> [Path] -> [[LocDirAndSteps]]
    go _ [] = []
    go stepCount currStates = finalLocsAndCount ++ go nextStepCount nextStates
      where
        finalLocsAndCount = map getFullPath finalStates
        (finalStates, nextStates) = partition ((== startLoc) . _currLoc) allNextStates
        allNextStates = nextStepCount `seq` concatMap (genNextStates stepCount) currStates
        nextStepCount = stepCount + 1

    -- Return the full path held along with the final location and direction. The final step count
    -- can be zero since we have returned to the starting location.

    getFullPath :: Path -> [LocDirAndSteps]
    getFullPath (Path currLoc currDir priorLocs) = (currLoc, currDir, 0) : priorLocs

    -- From the given path, generate a list of next states. There may be zero, if the only
    -- connection is to the prior location, one if there is a connection that is not the prior
    -- location, and two, if we are at the start and don't have a prior location.

    genNextStates :: Int -> Path -> [Path]
    genNextStates stepCount (Path currLoc currDir priorLocs) = nextStates
      where
        nextStates = map (\(loc, dir) -> Path loc dir nextPriorLocs) adjoiningLocsAndDirs
        nextPriorLocs = (currLoc, currDir, stepCount) : priorLocs
        adjoiningLocsAndDirs = findAdjoiningLocs priorLoc currLoc
        priorLoc = if null priorLocs then Nothing
                   else (Just . (\(loc, _, _) -> loc) . head) priorLocs

    -- Find the adjacent locations to this one where we can move. This function takes a prior
    -- location, which may be nothing, but if it is a location, make sure we don't return it as one
    -- of the next moves.

    findAdjoiningLocs :: Maybe Location -> Location -> [LocAndDir]
    findAdjoiningLocs priorLocM currLoc
      | isNothing priorLocM = validConnectingLocsAndDirs
      | otherwise = let priorLoc = fromJust priorLocM
                    in  filter ((/= priorLoc) . fst) validConnectingLocsAndDirs
      where
        validConnectingLocsAndDirs = filter (hasConnBack currLoc) possConns
        possConns = adjacentPossibleConnectionsWithDir mazeArray currLoc

    -- Return true if one of the connecting locations from the new location is the original
    -- location.

    hasConnBack :: Location -> LocAndDir -> Bool
    hasConnBack origLoc (newLoc, _)
      | origLoc `elem` connsFromNewLoc = True
      | otherwise = False
      where
        connsFromNewLoc = adjacentPossibleConnections mazeArray newLoc

-- From the given location, which must be in the bounds of the maze, generate a list of the
-- adjacent locations that it could connect to. For example, if thete is a vertical pipe at the
-- current location, this will return a list of the two locations above and below the current
-- one. This will filter out any locations outside of the maze range.

adjacentPossibleConnections :: MazeArray -> Location -> [Location]
adjacentPossibleConnections mazeArray loc
  = map fst $ adjacentPossibleConnectionsWithDir mazeArray loc

adjacentPossibleConnectionsWithDir :: MazeArray -> Location -> [LocAndDir]
adjacentPossibleConnectionsWithDir mazeArray loc@(x, y) = filter inRange resultAll
  where
    currElem = mazeArray UA.! loc
    resultAll = case currElem of
               Ground     -> []
               Start      -> [((x, y + 1), North), ((x, y - 1), South),
                              ((x + 1, y), East), ((x - 1, y), West)]
               Vertical   -> [((x, y + 1), North), ((x, y - 1), South)]
               Horizontal -> [((x + 1, y), East), ((x - 1, y), West)]
               LLBend     -> [((x + 1, y), East), ((x, y + 1), North)]
               LRBend     -> [((x - 1, y), West), ((x, y + 1), North)]
               URBend     -> [((x - 1, y), West), ((x, y - 1), South)]
               ULBend     -> [((x + 1, y), East), ((x, y - 1), South)]

    -- Return true if the location is within the bounds of the maze. Calling the 

    inRange :: LocAndDir -> Bool
    inRange (loc', _) = inMazeRange mazeArray loc'

-- Return all in-boundary locations North, East, South, and West of the current location.

adjacentValidLocations :: MazeArray -> Location -> [Location]
adjacentValidLocations mazeArr (x, y)
  = filter (inMazeRange mazeArr) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

-- Returns true if the given location is in range of the maze array passed in.

inMazeRange :: MazeArray -> Location -> Bool
inMazeRange mazeArray (x, y)
  | x < xLow || x > xHigh || y < yLow || y > yHigh = False
  | otherwise = True
  where
    ((xLow, yLow), (xHigh, yHigh)) = UA.bounds mazeArray

-- Find a single start location in the maze. If there are none or multiple, generate an error.

findStartLoc :: MazeArray -> Either String (Int, Int)
findStartLoc arr
  | null starts = Left "No start location in the maze."
  | (not . null . tail) starts = Left "Multiple start locations in the maze."
  | otherwise = Right ((fst . head) starts)
  where
    starts = (filter ((== Start) . snd) . UA.assocs) arr

-- Convert an input character to the type of maze element.

convSymbol :: Char -> Maybe MazeElem
convSymbol ch = case ch of
                  '.' -> Just Ground
                  'S' -> Just Start
                  '|' -> Just Vertical
                  '-' -> Just Horizontal
                  'L' -> Just LLBend
                  'J' -> Just LRBend
                  '7' -> Just URBend
                  'F' -> Just ULBend
                  _   -> Nothing

-- Given the location, direction facing, and element of the maze there, determine the locations that
-- would be to the left and right. This function does not filter based on the range of the maze.

determineLeftAndRightLocs :: Location -> Direction -> MazeElem -> ([Location], [Location])
determineLeftAndRightLocs (x, y) dir mazeElem
  = case dir of
      North   -> case mazeElem of
                   Vertical   -> ([(x - 1, y)], [(x + 1, y)])
                   LLBend     -> ([(x - 1, y), (x, y - 1)], [])
                   LRBend     -> ([], [(x, y - 1), (x + 1, y)])
                   URBend     -> ([], [(x, y + 1), (x + 1, y)])
                   ULBend     -> ([(x - 1, y), (x, y + 1)], [])
                   _          -> ([], [])
      East    -> case mazeElem of
                   Horizontal -> ([(x, y + 1)], [(x, y - 1)])
                   LLBend     -> ([], [(x - 1, y), (x, y - 1)])
                   LRBend     -> ([], [(x, y - 1), (x + 1, y)])
                   URBend     -> ([(x, y + 1), (x + 1, y)], [])
                   ULBend     -> ([(x - 1, y), (x, y + 1)], [])
                   _          -> ([], [])
      South   -> case mazeElem of
                   Vertical   -> ([(x + 1, y)], [(x - 1, y)])
                   LLBend     -> ([], [(x - 1, y), (x, y - 1)])
                   LRBend     -> ([(x, y - 1), (x + 1, y)], [])
                   URBend     -> ([(x, y + 1), (x + 1, y)], [])
                   ULBend     -> ([], [(x - 1, y), (x, y + 1)])
                   _          -> ([], [])
      West    -> case mazeElem of
                   Horizontal -> ([(x, y - 1)], [(x, y + 1)])
                   LLBend     -> ([(x - 1, y), (x, y - 1)], [])
                   LRBend     -> ([(x, y - 1), (x + 1, y)], [])
                   URBend     -> ([], [(x, y + 1), (x + 1, y)])
                   ULBend     -> ([], [(x - 1, y), (x, y + 1)])
                   _          -> ([], [])
      Unknown -> ([], [])
