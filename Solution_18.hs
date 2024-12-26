-- For Advent of Code 2023
--
-- By Truman Collins
-- December 2023

module Solution_18 (
  puzzle_18
) where

import Data.List
import qualified Data.Array as A
import qualified Data.Array.ST as STA
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Utilities
import Parsers

data Direction = UpDir | RightDir | DownDir | LeftDir deriving (Eq, Show)

data Side = LeftSide | RightSide deriving (Eq, Show)

data MapElem = Ground | Trench deriving (Eq, Show)

data Move = Move { _dir   :: Direction
                 , _dist  :: Int
                 , _color :: String
                 } deriving Show

type MapArr = A.Array Location MapElem

data ExtentData = ExtentData { _currLoc  :: Location
                             , _currDir  :: Direction
                             , _turnSum  :: Int
                             , _bounds   :: (Location, Location)
                             , _pathLocs :: [LocAndMoveDir]
                             } deriving Show

type Location = (Int, Int)
type LocAndMoveDir = (Location, Direction)


-- Constants for this puzzle.

puzzNum :: Int
puzzNum =  18
inputFile :: String
inputFile = "puzzle_18.inp"

--
-- Code for Puzzle 18.
--

puzzle_18 :: IO (Int, Int)
puzzle_18 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzleInput <- fmap (map (parse readInputLine) . lines) (readFile inputFile)

  -- Make sure the input was parsed correctly and retrive it out of the returned parser data
  -- structure. If an error is found, a message will be generated and the program will halt.

  moveInstructs <- validateAndReturnParseLinesResult puzzleInput inputFile puzzNum

  when (null moveInstructs)
       (ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile "Empty instructions."))

  let startLoc = (0, 0)
      startDir = (_dir . head) moveInstructs
      initialExtent = ExtentData startLoc startDir 0 (startLoc, startLoc) [(startLoc, startDir)]
      extentData = foldl' accPath initialExtent moveInstructs
      filledMapArr = runST $ createAndClearMapArr extentData

  let ansPart1 = (length . filter (== Trench) . A.elems) filledMapArr

  return (ansPart1, 43)

createAndClearMapArr :: ExtentData -> ST s MapArr
createAndClearMapArr extentData = do
  mapArr <- STA.newArray (_bounds extentData) Ground :: ST s (STA.STArray s (Int, Int) MapElem)
  mapM_ ((\loc -> STA.writeArray mapArr loc Trench) . fst) (_pathLocs extentData)
  let side = if _turnSum extentData >= 0 then RightSide else LeftSide
  mapM_ (fillInTrench mapArr side) (_pathLocs extentData)
  STA.freeze mapArr

  where
    fillInTrench :: STA.STArray s (Int, Int) MapElem -> Side -> LocAndMoveDir -> ST s ()
    fillInTrench mapArr side ((x, y), dir) = do
      let sideLocs = filter (inMapBounds (_bounds extentData)) [sideLoc]
          sideLoc = case (side, dir) of
            (LeftSide, UpDir) -> (x - 1, y)
            (LeftSide, RightDir) -> (x, y + 1)
            (LeftSide, DownDir) -> (x + 1, y)
            (LeftSide, LeftDir) -> (x, y - 1)
            (RightSide, UpDir) -> (x + 1, y)
            (RightSide, RightDir) -> (x, y - 1)
            (RightSide, DownDir) -> (x - 1, y)
            (RightSide, LeftDir) -> (x, y + 1)
      mapM_ (fillAndRecurse mapArr) sideLocs
      where
        fillAndRecurse :: STA.STArray s (Int, Int) MapElem -> Location -> ST s ()
        fillAndRecurse mapArr' loc = do
          currVal <- STA.readArray mapArr' loc
          when (currVal == Ground) $ do
            STA.writeArray mapArr' loc Trench
            let oneStepAways = genOneStepAways loc
            mapM_ (fillAndRecurse mapArr') oneStepAways
          where
            genOneStepAways :: Location -> [Location]
            genOneStepAways (x', y') = [(x' + 1, y'), (x' - 1, y'), (x', y' + 1), (x', y' - 1)]
            

accPath :: ExtentData -> Move -> ExtentData
accPath (ExtentData currLoc currDir currTurnSum currBounds currPathLocs) (Move dir dist _color)
  = ExtentData newLoc newDir newTurnSum newBounds newPathLocs
  where
    (newLoc, locsTo) = computeNewLoc currLoc dir dist
    newDir = dir
    newTurnSum = currTurnSum + computeTurnVal currDir newDir
    newBounds  = expandBounds currBounds newLoc
    newPathLocs = currPathLocs ++ locsTo

computeTurnVal :: Direction -> Direction -> Int
computeTurnVal UpDir UpDir = 0
computeTurnVal UpDir RightDir = 1
computeTurnVal UpDir DownDir = error (genErrStrForError "Direction reversal.")
computeTurnVal UpDir LeftDir = -1
computeTurnVal RightDir UpDir = -1
computeTurnVal RightDir RightDir = 0
computeTurnVal RightDir DownDir = 1
computeTurnVal RightDir LeftDir = error (genErrStrForError "Direction reversal.")
computeTurnVal DownDir UpDir = error (genErrStrForError "Direction reversal.")
computeTurnVal DownDir RightDir = -1
computeTurnVal DownDir DownDir = 0
computeTurnVal DownDir LeftDir = 1
computeTurnVal LeftDir UpDir = 1
computeTurnVal LeftDir RightDir = error (genErrStrForError "Direction reversal.")
computeTurnVal LeftDir DownDir = -1
computeTurnVal LeftDir LeftDir = 0

expandBounds :: ((Int, Int), (Int, Int)) -> (Int, Int) -> ((Int, Int), (Int, Int))
expandBounds ((xLow, yLow), (xHigh, yHigh)) (x, y) = xLowNew `seq` ((xLowNew, yLowNew), (xHighNew, yHighNew))
  where
    xLowNew  = yLowNew `seq` min xLow x
    yLowNew  = xHighNew `seq` min yLow y
    xHighNew = yHighNew `seq` max xHigh x
    yHighNew = max yHigh y

computeNewLoc :: Location -> Direction -> Int -> (Location, [LocAndMoveDir])
computeNewLoc (currX, currY) UpDir dist
  = ((currX, currY + dist), [((currX, y), UpDir) | y <- [(currY + 1)..(currY + dist)]])
computeNewLoc (currX, currY) RightDir dist
  = ((currX + dist, currY), [((x, currY), RightDir) | x <- [(currX + 1)..(currX + dist)]])
computeNewLoc (currX, currY) DownDir dist
  = ((currX, currY - dist), [((currX, y), DownDir) | y <- [(currY - 1), (currY - 2)..(currY - dist)]])
computeNewLoc (currX, currY) LeftDir dist
  = ((currX - dist, currY), [((x, currY), LeftDir) | x <- [(currX - 1), (currX - 2)..(currX - dist)]])

-- Check if the location of this state is within the bounds of the map.

inMapBounds :: ((Int, Int), (Int, Int)) -> Location -> Bool
inMapBounds ((xMin, yMin), (xMax, yMax)) (x, y)
  | x < xMin || x > xMax = False
  | y < yMin || y > yMax = False
  | otherwise = True

-- Functions to parse the input lines.

readInputLine :: Parser Move
readInputLine = do
  dirCh <- sat isDirChar
  _ <- space
  distVal <- int
  _ <- space
  _ <- symbol "(#"
  hexStr <- some hexDigit
  _ <- symbol ")"
  return (Move (convToDir dirCh) distVal hexStr)
  where
    isDirChar 'U' = True
    isDirChar 'R' = True
    isDirChar 'D' = True
    isDirChar 'L' = True
    isDirChar _   = False

    convToDir 'U' = UpDir
    convToDir 'R' = RightDir
    convToDir 'D' = DownDir
    convToDir 'L' = LeftDir
    convToDir _ = error (genErrStrForError "Bad direction character.")

genErrStrForError :: String -> String
genErrStrForError = genErrStrForErrorFn puzzNum inputFile
