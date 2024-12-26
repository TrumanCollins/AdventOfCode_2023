-- For Advent of Code 2023
--
-- By Truman Collins
-- December 2023

module Solution_02 (
  puzzle_02
) where

import Data.List
import Control.Applicative
import Parsers

data GameResult = GameResult { _id    :: Int
                             , _cubeCounts :: [CubesRGB]
                             } deriving Show

data Color = Red | Green | Blue deriving (Show, Eq)

data CubesRGB = CubesRGB { _red   :: Int
                         , _green :: Int
                         , _blue  :: Int
                         } deriving Show


-- Constants for this puzzle.

puzzNum :: Int
puzzNum =  2
inputFile :: String
inputFile = "puzzle_02.inp"
totalCubeCount :: CubesRGB
totalCubeCount =  CubesRGB 12 13 14

--
-- Code for Puzzle 02.
--

puzzle_02 :: IO (Int, Int)
puzzle_02 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzleInput <- fmap (map (parse readInputLine) . lines) (readFile inputFile)

  -- Make sure the input was parsed correctly and retrive it out of the returned parser data
  -- structure. If an error is found, a message will be generated and the program will halt.

  gameResults <- validateAndReturnParseLinesResult puzzleInput inputFile puzzNum

  -- Compute the answer for part 1.

  let ansPart1 = (sum . map _id . filter (possibleDraws totalCubeCount)) gameResults

  -- Compute the answer for part 2.

  let ansPart2 = (sum . map (computePower . findMinCubeSet)) gameResults

  return (ansPart1, ansPart2)

-- Figure out the minimum cube set needed to play the given game.

findMinCubeSet :: GameResult -> CubesRGB
findMinCubeSet (GameResult _ cubeGroups) = foldl' accMaxCubeCounts (CubesRGB 0 0 0) cubeGroups
  where
    accMaxCubeCounts :: CubesRGB -> CubesRGB -> CubesRGB
    accMaxCubeCounts (CubesRGB accRed accGreen accBlue) (CubesRGB red green blue)
      = newRed `seq` CubesRGB newRed newGreen newBlue
      where
        newRed   = newGreen `seq` max accRed red
        newGreen = newBlue `seq` max accGreen green
        newBlue  = max accBlue blue
  
-- Compute the power of a cube set, which is the product of the counts.

computePower :: CubesRGB -> Int
computePower (CubesRGB red green blue) = product [red, green, blue]

-- Return true if the game was possible given the cube counts given for the bag.

possibleDraws :: CubesRGB -> GameResult -> Bool
possibleDraws (CubesRGB totRed totGreen totBlue) (GameResult _ draws) = all possibleDraw draws
  where
    possibleDraw (CubesRGB red green blue)
      | red > totRed = False
      | green > totGreen = False
      | blue > totBlue = False
      | otherwise = True

-- Functions to parse the input lines.

readInputLine :: Parser GameResult
readInputLine = do
  _ <- symbol "Game"
  _ <- space
  idNum <- int
  _ <- symbol ":"
  GameResult idNum <$> readSSLCubes

-- Read a semicolon-separated list of cube counts, and return the list of them.

readSSLCubes :: Parser [CubesRGB]
readSSLCubes = do
  cube <- readCSLColorCounts
  cubes <- many readSemiCube
  return (cube : cubes)

readSemiCube :: Parser CubesRGB
readSemiCube = do
  _ <- symbol ";"
  readCSLColorCounts

-- Read a comma-separated list of counts and colors, returning a CubesRGB object holding the sum of
-- the counts.

readCSLColorCounts :: Parser CubesRGB
readCSLColorCounts = do
  colCnt <- readCountColor
  colCntList <- many readCommaCountColor
  return (foldl' addIntoCubes (CubesRGB 0 0 0) (colCnt : colCntList))
  where
    addIntoCubes :: CubesRGB -> (Int, Color) -> CubesRGB
    addIntoCubes cubes (cnt, col)
      | col == Red = cubes {_red = cnt + _red cubes}
      | col == Green = cubes {_green = cnt + _green cubes}
      | col == Blue = cubes {_blue = cnt + _blue cubes}
      | otherwise = error ("Error: unknown color " ++ show col)

readCommaCountColor :: Parser (Int, Color)
readCommaCountColor = do
  _ <- symbol ","
  readCountColor

-- Read a count and then a color and return as a pair.

readCountColor :: Parser (Int, Color)
readCountColor = do
  _ <- space
  count <- int
  _ <- space
  color <- readColor
  return (count, color)

-- Read a color from the input, red, green, or blue, and return the enum for it. Assume all
-- characters are lower case.

readColor :: Parser Color
readColor = do
    _ <- symbol "red"
    return Red
  <|> do
    _ <- symbol "green"
    return Green
  <|> do
    _ <- symbol "blue"
    return Blue
