 -- For Advent of Code 2023
--
-- By Truman Collins
-- December 2023

module Solution_04 (
  puzzle_04
) where

import qualified Data.Set as S
import qualified Data.Array.ST as STA
import Control.Monad
import Control.Monad.ST
import Utilities

data CardInfo = CardInfo { _id          :: Int
                         , _winningNums :: S.Set Int
                         , _haveNums    :: S.Set Int
                         , _matches     :: Int
                         } deriving Show

-- Constants for this puzzle.

puzzNum :: Int
puzzNum =  4
inputFile :: String
inputFile = "puzzle_04.inp"

--
-- Code for Puzzle 04.
--

puzzle_04 :: IO (Int, Int)
puzzle_04 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInput <- fmap lines (readFile inputFile)

  -- Check for errors in the input.

  let (errorInInput, errorMsg) = checkForInputErrors puzzInput
  when errorInInput
       (ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile errorMsg))

  -- Convert the input to a list of card infos.
  
  let cardInfos = map genCardInfo puzzInput
      initialCardCount = length cardInfos
      lastCardNum = (_id . last) cardInfos

  when (initialCardCount /= lastCardNum)
       (ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile "Card ID problem."))

  -- Compute the answer for part 1.

  let ansPart1 = (sum . map computePoints) cardInfos

  -- Compute the answer for part 2.

  let ansPart2 = runST $ computeAnsPart2 cardInfos initialCardCount

  return (ansPart1, ansPart2)

-- Compute the answer for part 2, and use the ST monad for efficiency.

computeAnsPart2 :: [CardInfo] -> Int -> ST s Int
computeAnsPart2 initialCards cardCount = do
  cardCountArr <- STA.newArray (1, cardCount) 1 :: ST s (STA.STUArray s Int Int)

  -- Go through the cards, adding to the card count for higher ID cards as appropriate.

  forM_ initialCards $ \card -> do
    let cardID  = _id card
        matches = _matches card
    cardsThisID <- STA.readArray cardCountArr cardID

    -- Add the appropriate number of cards to future counts.

    forM_ [(cardID + 1)..(min cardCount (cardID + matches))] $ \i -> do
      currCardCount <- STA.readArray cardCountArr i
      STA.writeArray cardCountArr i (currCardCount + cardsThisID)
    
  -- Sum up the number of cards associated with each ID, and return this sum.
  -- It can also be done with forM_, but this is simpler.

  foldM (\acc i -> do
            value <- STA.readArray cardCountArr i
            return $! acc + value
        ) 0 [1..cardCount]
  
-- Returns the number of point associated with this card for part 1.

computePoints :: CardInfo -> Int
computePoints cardInfo
    | winningCount == 0 = 0
    | otherwise = 2 ^ (winningCount - 1)
  where
    winningCount = _matches cardInfo

-- Generate a CardInfo record from the input string. Ideally, error checking would be done here.

genCardInfo :: String -> CardInfo
genCardInfo str = CardInfo ((read . init) idStr) winningNums haveNums matches
  where
    matches     = S.size $ S.intersection winningNums haveNums
    winningNums = (S.fromList . map read) winningStrs
    haveNums    = (S.fromList . map read) haveStrs
    (winningStrs, _ : haveStrs) = span (/= "|") remainder
    (_ : idStr : remainder) = words str

-- Check for errors in the input lines.

checkForInputErrors :: [String] -> (Bool, String)
checkForInputErrors inputStrings
  | null inputStrings || any null inputStrings = (True, "Empty input or empty lines.")
  | otherwise = (False, "")
