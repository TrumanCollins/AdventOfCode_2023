-- Tests for Utilities.hs
-- By Truman Collins
-- May 25, 2024
--
-- See https://www.fpcomplete.com/blog/2017/01/quickcheck/
-- and Test.QuickCheck for reference.
--
-- These tests are designed to use hspec to allow specific tests as well as property tests.
-- Note that when defining a property, you can use types that have constraints such as (SortedList
-- a) or (NonNegative Int) to refine the input. Note the data declaration (Sorted a). See
-- propSameAsHeadGroup.
-- You can determine some inputs as parameters to the function, and other inside the function with
-- forAll.

import Data.Bits
import qualified Data.Set as Set
import Test.Hspec

import Utilities

main :: IO ()
main = do
  testStateMachEven2sAnd3s
  testHexCountWith01A
  test10DigitNumbers
  putStrLn "Completed tests."


data StateEven2And3 = StateEven2And3 { _evenTwos   :: Bool
                                     , _evenThrees :: Bool
                                     } deriving (Eq, Ord, Show)

testStateMachEven2sAnd3s :: IO ()
testStateMachEven2sAnd3s = hspec $ do
  describe "State Machine for even 2 and 3 count." $ do
    mapM_ doTest testList
  where

    doTest :: (String, Bool) -> SpecWith ()
    doTest (str, ans) = it ("String: " ++ str) (acceptable str `shouldBe` ans)

    testList = [("121131121113", True), ("", True), ("1112111", False), ("113113113", False)]

    -- Generate the state vector for the state machine that recognizes strings involving '1', '2',
    -- and '3', where acceptable strings have an even number of 2s and an even number of 3s.

    stVec = genStateVec "123" (StateEven2And3 True True) isEven2sAnd3s nextStatesEven2sAnd3s

    -- Shorthand for seeing if the string is acceptable.

    acceptable :: String -> Bool
    acceptable = isAcceptSymbolList stVec

    -- The accept function.

    isEven2sAnd3s :: StateEven2And3 -> Bool
    isEven2sAnd3s (StateEven2And3 evenTwos evenThrees) = evenTwos && evenThrees

    -- The function to generate the next set of states from the current one given the current state
    -- and a symbol.

    nextStatesEven2sAnd3s :: Char -> StateEven2And3 -> [StateEven2And3]
    nextStatesEven2sAnd3s ch incomingState@(StateEven2And3 evenTwos evenThrees)
      | ch == '2' = [StateEven2And3 (not evenTwos) evenThrees]
      | ch == '3' = [StateEven2And3 evenTwos (not evenThrees)]
      | otherwise = [incomingState]
    
testHexCountWith01A :: IO ()
testHexCountWith01A = hspec $ do
  describe "Count all hex up to 16 digits with 0, 1, and A present." $ do
    let stateVec = genStateVec "0123456789ABCDEF" (Set.empty, True) isAccept addSymbolFn
        count = (sum . map sumFinalCounts . take 17 ) (statesAndCountsStartingAtState0 stateVec)
        sumFinalCounts = sum . map dfaCount_r . filter (isAcceptState stateVec)
    it "Count: " (count `shouldBe` 4420408745587516162)
  where
    isAccept (currSet, _) = Set.size currSet == 3
    addSymbolFn ch (currSet, firstChar)
      | ch == '0' && firstChar = []
      | ch == '0' || ch == '1' || ch == 'A' = [(Set.insert ch currSet, False)]
      | otherwise = [(currSet, False)]

test10DigitNumbers :: IO ()
test10DigitNumbers = hspec $ do
  describe "Count all 10 digit numbers with no digit occurring more than 3 times." $ do
    let stateVec = genStateVec [0..9] (0 :: Int) isAccept addDigitFn
        count = sumFinalCountsAfterNSteps stateVec 10
    it "Count: " (count `shouldBe` 7857440640)
  where
    isAccept = const True

    -- When we go to add a digit, we don't count a leading zero, and we don't add a digit if there
    -- have already been 3 of these digits. Otherwise the new state has an incremented digit count
    -- for the added digit.

    addDigitFn val currState
      | currState == 0 && val == 0 = []
      | currCount == 3 = []
      | otherwise = let shiftedOne = shiftL 0x1 shiftCount
                        newState = currState + shiftedOne
                    in  newState `seq` [newState]
      where
        currCount = shiftR (currState .&. shiftMask) shiftCount
        shiftMask = shiftL 0x3 shiftCount
        shiftCount = val + val
