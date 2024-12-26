-- For Advent of Code 2023
--
-- By Truman Collins
-- December 2023

module Solution_07 (
  puzzle_07
) where

import Data.List
import Data.Tuple (swap)
import Data.Maybe
import qualified Data.Map.Strict as M
import Text.Read (readMaybe)
import Control.Monad
import Utilities

-- Types for this puzzle.

data HandType = HighCard | OnePair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind
                deriving (Show, Eq, Ord, Enum)
data Card = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack
            | Queen | King | Ace deriving (Show, Eq, Ord, Enum)

data HandData = HandData { _handType     :: HandType
                         , _handTypeP2   :: HandType
                         , _origHand     :: CardHand
                         , _handPart2    :: CardHand
                         , _bid          :: Bid
                         } deriving Show

type CardHand = [Card]
type Bid = Int
type CardCountMap = M.Map Card Int
type CardAndCount = (Card, Int)
type CardCountList = [CardAndCount]

-- Constants for this puzzle.

puzzNum :: Int
puzzNum =  7
inputFile :: String
inputFile = "puzzle_07.inp"

--
-- Code for Puzzle 07.
--

puzzle_07 :: IO (Int, Int)
puzzle_07 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInput <- fmap lines (readFile inputFile)

  -- Convert each line to a record, and check for errors at the same time.

  let maybeHands = map readConvAndErrorCheck puzzInput
      errorInInput = any isNothing maybeHands

  when errorInInput
       (ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile "Invalid input file."))

  -- Convert the input lines to a list of hands stored in HandData.

  let hands = (map buildHandRec . catMaybes) maybeHands

  let ansPart1 = rankHandsAndSumProdOfRanksAndBids _handType _origHand hands
  let ansPart2 = rankHandsAndSumProdOfRanksAndBids _handTypeP2 _handPart2 hands

  return (ansPart1, ansPart2)

-- Sort the hands based on the hand type and cards, which are retrieved from the hand data using
-- functions so this can be used by both part 1 and part 2.

rankHandsAndSumProdOfRanksAndBids :: (HandData -> HandType) -> (HandData -> CardHand) -> [HandData]
                                     -> Int
rankHandsAndSumProdOfRanksAndBids handTypeFn cardHandFn hands = (sum . zipWith (*) [1..]) ordBids
  where
    ordBids = (map getBid . sort . map (\h -> (handTypeFn h, cardHandFn h, _bid h))) hands
    getBid (_, _, bid) = bid

-- Given a hand of cards and a bid, build the data for a hand, including the hand type for both
-- parts 1 and 2 of the problem,

buildHandRec :: (CardHand, Bid) -> HandData
buildHandRec (cardHand, bid) = HandData handTypeP1 handTypeP2 cardHand cardHandP2 bid
  where

    -- Figuring out the hand we have is easy for part 1 given the number of different cards and the
    -- sorted list of cards and counts.

    handTypeP1 = determineHandType diffCardCountP1 sortedByCountThenCard

    -- Determining the type of hand for part 2 is a little harder. We need to separate out any
    -- Jacks/Jokers. If there are only Jokers, then we have five of a kind with jokers. If there are
    -- no Jokers, then the hand type is the same as for part 1. In other cases, we take the Joker
    -- count and add it to the other kind of card with the highest count in the hand, then evaluate
    -- this new hand.

    handTypeP2
      | null jokers = handTypeP1
      | null notJokers = FiveKind
      | otherwise = let jokerCount = (snd . head) jokers
                        (topCard, topCount) : others = notJokers
                        diffCardCountP2 = diffCardCountP1 - 1
                        newTopCardCount = topCount + jokerCount
                    in  determineHandType diffCardCountP2 ((topCard, newTopCardCount) : others)
    (jokers, notJokers) = (partition isJoker . map jackToJokerPair) sortedByCountThenCard

    -- We want to convert the Jacks to Jokers in the list of cards dealt so we can use it to order
    -- the hands for part 2 of this problem.

    cardHandP2 = map jackToJoker cardHand

    -- Insert the cards in this hand into a map to count the number of each card. Convert the map to
    -- a list, note the number of different cards, and sort the list first by count then by card.

    cardCountMap = foldl' insertOrIncCard M.empty cardHand
    sortedByCountThenCard = sortByCountThenCard mapList
    mapList = M.toList cardCountMap
    diffCardCountP1 = M.size cardCountMap

    -- Evaluate the list of (card, count) pairs to determine the type of hand. The list has been
    -- ordered by count first then the card.

    determineHandType :: Int -> CardCountList -> HandType
    determineHandType diffCardCount sortedCountsAndCards
      | diffCardCount == 1 = FiveKind
      | diffCardCount == 2 = if (snd . head) sortedCountsAndCards == 4
                             then FourKind
                             else FullHouse
      | diffCardCount == 3 = if (snd . head) sortedCountsAndCards == 3
                             then ThreeKind
                             else TwoPair
      | diffCardCount == 4 = OnePair
      | otherwise = HighCard

    -- Convert a Jack to a Joker, and leave the other cards unchanged.

    jackToJoker :: Card -> Card
    jackToJoker Jack = Joker
    jackToJoker card = card

    -- Jack to joker conversion in a pair with the count of these cards.

    jackToJokerPair :: CardAndCount -> CardAndCount
    jackToJokerPair (Jack, count) = (Joker, count)
    jackToJokerPair pair = pair

    isJoker :: CardAndCount -> Bool
    isJoker (Joker, _) = True
    isJoker _ = False

    -- Insert or increment the counter for a card in the map.

    insertOrIncCard :: CardCountMap -> Card -> CardCountMap
    insertOrIncCard cardMap card = M.insertWith (+) card 1 cardMap

    -- Sort the list of cards and counts by count first then cards. Could make a custom compare
    -- function so we wouldn't have to swap or reverse, but this list will be at most 5 elements
    -- long.

    sortByCountThenCard :: CardCountList -> CardCountList
    sortByCountThenCard = map swap . reverse . sort . map swap

-- Convert a single line of input to a list of the cards in the hand and the bid. Error check for
-- valid cards, 5 cards, numeric bid, and two inputs.

readConvAndErrorCheck :: String -> Maybe (CardHand, Bid)
readConvAndErrorCheck inputLine
  | inputItemCount /= 2 = Nothing
  | isNothing bidMaybe = Nothing
  | any isNothing maybeCards = Nothing
  | length cardList /= 5 = Nothing
  | otherwise = Just (cardList, fromJust bidMaybe)
  where
    cardList = catMaybes maybeCards
    maybeCards = map convToMaybeCard cardChars
    bidMaybe = readMaybe bidStr
    cardChars = head wordList
    bidStr = (head . tail) wordList
    inputItemCount = length wordList
    wordList = words inputLine

    -- Convert a character to a card, returning Nothing if it's not in the valid set.

    convToMaybeCard :: Char -> Maybe Card
    convToMaybeCard ch = case ch of
                           '2' -> Just Two
                           '3' -> Just Three
                           '4' -> Just Four
                           '5' -> Just Five
                           '6' -> Just Six
                           '7' -> Just Seven
                           '8' -> Just Eight
                           '9' -> Just Nine
                           'T' -> Just Ten
                           'J' -> Just Jack
                           'Q' -> Just Queen
                           'K' -> Just King
                           'A' -> Just Ace
                           _   -> Nothing
