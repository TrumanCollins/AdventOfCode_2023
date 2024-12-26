-- For Advent of Code 2023
--
-- By Truman Collins
-- October 2024

{-# LANGUAGE MultiWayIf #-}

module Solution_19 (
  puzzle_19
) where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Array as A
import Control.Applicative
import Control.Monad
import Parsers
import Utilities

data RulesAndParts = RulesAndParts { _ruleSets :: [RuleSet]
                                   , _parts    :: [Part]
                                   } deriving Show

data RatedFor = X | M | A | S deriving (Eq, Show)

data Part = Part { _x  :: Int
                 , _m  :: Int
                 , _a  :: Int
                 , _s  :: Int
                 } deriving Show

-- This is used processing part 1.

data PartAndRule = PartAndRule { _ruleNumber :: Int
                               , _part       :: Part
                               } deriving Show

type StateOfParts = ([PartAndRule], [PartAndRule], [PartAndRule])

-- This is used processing part 2, where we need to keep track of ranges.

type SingleRange = (Int, Int)

initRange :: SingleRange
initRange = (1, 4000)

data XmasRanges = XmasRanges { _xRange :: SingleRange
                             , _mRange :: SingleRange
                             , _aRange :: SingleRange
                             , _sRange :: SingleRange
                             } deriving Show
initXmasRanges :: XmasRanges
initXmasRanges = XmasRanges initRange initRange initRange initRange

data PartRuleRange = PartRuleRange { _ruleNo   :: Int
                                   , _ranges   :: XmasRanges
                                   } deriving Show

type StateOfRanges = ([PartRuleRange], [PartRuleRange], [PartRuleRange])

type AcceptedAndRejected = ([Part], [Part])

type AcceptedAndRejectedRanges = ([PartRuleRange], [PartRuleRange])

data RuleSet = RuleSet { _name   :: String
                       , _id     :: Int
                       , _rules  :: [Rule]
                       } deriving Show

data Rule = Accept | Reject | RuleConst (String, Int)
            | RuleCond (Part -> Bool, RatedFor, Ordering, Int) (String, Int)
instance Show Rule where
  show Accept = "Accept"
  show Reject = "Reject"
  show (RuleConst (str, idNum)) = "RuleConst (" ++ show str ++ ", " ++ show idNum ++ ")"
  show (RuleCond (_, rf, ordering, val) (str, idNum)) = "RuleCond (" ++ show rf ++ ", "
         ++ show ordering ++ ", " ++ show val ++ ") (" ++ show str ++ ", " ++ show idNum ++ ")"

type RuleSetArr = A.Array Int RuleSet
type RuleSetIndMap = M.Map String Int


-- Constants for this puzzle.

puzzNum :: Int
puzzNum =  19
inputFile :: String
inputFile = "puzzle_19.inp"

acceptIndex :: Int
acceptIndex = 0
rejectIndex :: Int
rejectIndex = 1

--
-- Code for Puzzle 19.
--

puzzle_19 :: IO (Int, Int)
puzzle_19 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzleInput <- fmap (parse parseInputFile) (readFile inputFile)

  -- Make sure the input was parsed correctly and retrive it out of the returned parser data
  -- structure. If an error is found, a message will be generated and the program will halt.

  rulesAndPartsRaw <- validateAndReturnParseResult puzzleInput inputFile puzzNum

  let (rulesAndParts, ruleSetArr, arrIndMap) = createRuleSetData rulesAndPartsRaw
      inIndexM = M.lookup "in" arrIndMap

  -- There needs to be a rule set named "in", as that's the point all the parts begin.
  
  when (isNothing inIndexM)
       (ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile "No entry for 'in'."))

  let inIndex = fromJust inIndexM
      initialParts = _parts rulesAndParts
      initialPartStates = map (PartAndRule inIndex) initialParts
      (accParts, _) = fullyProcessParts ruleSetArr initialPartStates
      ansPart1 = (sum . map addRatings) accParts

  let initialRanges = [PartRuleRange inIndex initXmasRanges]
      (accPartRanges, _) = fullyProcessPartRanges ruleSetArr initialRanges
      ansPart2 = (sum . map (computeCombinations . _ranges)) accPartRanges

  return (ansPart1, ansPart2)

-- Given a set of four ranges associated with the four letters XMAS, compute the total number of
-- combinations of these four ratings and return this number.

computeCombinations :: XmasRanges -> Int
computeCombinations (XmasRanges (xLow, xHigh) (mLow, mHigh) (aLow, aHigh) (sLow, sHigh))
  = (product . countRanges) allVals
  where
    allVals = [xLow, xHigh, mLow, mHigh, aLow, aHigh, sLow, sHigh]

    -- For a low and high pair, which will be next to each other in the list, compute the number
    -- that fall into that range.

    countRanges :: [Int] -> [Int]
    countRanges [] = []
    countRanges [_] = error (genErrStrForError "Odd number of XMAS ranges.")
    countRanges (low : high : rest) = (high - low + 1) : countRanges rest

-- This function is for part two, and it takes the array of rules sets as well as a list of initial
-- possible ranges for a part. It then works its way through the maze of rule sets dividing up the
-- ranges into separate pieces based on the conditional rules. Ultimately the initial set of four
-- ranges, one for each XMAS rating, having the range 1 to 4000, will be broken up into many pieces
-- and each piece will follow a path through the rule sets to an ultimate acceptance or rejection
-- and once these have all made their way, one step at a time through recursion, we return the set
-- of accepted and rejected range sets.

fullyProcessPartRanges :: RuleSetArr -> [PartRuleRange] -> AcceptedAndRejectedRanges
fullyProcessPartRanges ruleSetArr partRanges = go ([], [], partRanges)
  where

    go :: StateOfRanges -> AcceptedAndRejectedRanges
    go (acc, rej, []) = (acc, rej)
    go (acc, rej, currPRange : otherPRanges)

      -- We may have a part range that arrives on this list of ones to process that has been
      -- accepted or rejected. This can happen from a conditional resulting in accept or reject. It
      -- is simpler to move it to the right place here than in the processRules function, which is
      -- complicated enough as it is.

      | ruleIndex == acceptIndex = go (currPRange : acc, rej, otherPRanges)
      | ruleIndex == rejectIndex = go (acc, currPRange : rej, otherPRanges)
      | otherwise = go rangesAfterThisStep
      where
        rangesAfterThisStep = processRules (acc, rej, otherPRanges) (_ranges currPRange) ruleList
        ruleList  = _rules (ruleSetArr A.! ruleIndex)
--        ruleToUse = ruleSetArr A.! ruleIndex
        ruleIndex = _ruleNo currPRange

        processRules :: StateOfRanges -> XmasRanges -> [Rule] -> StateOfRanges

        -- We should never run out of rules, because the last one should be an accept, reject, or
        -- send us to a different rule.

        processRules _ _ [] = error (genErrStrForError "Ran out of rules.")

        -- If we are at an accept rule, then put this set of ranges on the accept list.

        processRules (currAcc, currRej, currFurther) xmasRanges (Accept : _)
          = (PartRuleRange acceptIndex xmasRanges : currAcc, currRej, currFurther)

        -- If we are at a reject rule, then put this set of ranges on the reject list.

        processRules (currAcc, currRej, currFurther) xmasRanges (Reject : _)
          = (currAcc, PartRuleRange rejectIndex xmasRanges : currRej, currFurther)

        -- When we run across another rule name with no condition, put this set of ranges on the
        -- list of range sets to process with the index of this new rule set. In the recursion after
        -- this is returned, this set of ranges will be processed next.

        processRules (currAcc, currRej, currFurther) xmasRanges ((RuleConst (_, ind)) : _)
          = (currAcc, currRej, PartRuleRange ind xmasRanges : currFurther)

        -- Here is the complicated case. We have a conditional rule. We need to find the individual
        -- range (XMAS) that is used, and see how the conditional relates to the current range. We
        -- consider less than and greater than conditions separately, and for each determine if the
        -- condition splits the range into two or not. If the condition doesn't apply at all, then
        -- continue with this set of ranges to the next rule in the rule set. If the contition
        -- covers the whole range, then get the new rule index and place this range set on the list
        -- to process recursively later. If the condition splits the range, then we do both of these
        -- things. Lots of combining and collapsing was done to simplify these decisions.

        processRules currStates@(currAcc, currRej, currFurther) xmasRanges
            ((RuleCond (_, ratedFor, ordr, val) (_, ind)) : remainRules)
          | ordr == LT
            = let matchedRanges = setIndividualRange (lowRange, min highRange (val - 1))
                  unmatchedRanges = setIndividualRange (max lowRange val, highRange)
                  newFurther = PartRuleRange ind matchedRanges : currFurther
                  newStates = (currAcc, currRej, newFurther)
              in  if | val <= lowRange  -> processRules currStates unmatchedRanges remainRules
                     | val > highRange -> newStates
                     | otherwise -> processRules newStates unmatchedRanges remainRules
          | ordr == GT
            = let matchedRanges = setIndividualRange (max lowRange (val + 1), highRange)
                  unmatchedRanges = setIndividualRange (lowRange, min highRange val)
                  newFurther = PartRuleRange ind matchedRanges : currFurther
                  newStates = (currAcc, currRej, newFurther)
              in  if | val >= highRange -> processRules currStates unmatchedRanges remainRules
                     | val < lowRange  -> newStates
                     | otherwise -> processRules newStates unmatchedRanges remainRules          
          | otherwise = error (genErrStrForError "Comparison other than LT or GT not allowed.")
          where

            -- The range we are working with in this function.

            (lowRange, highRange) = rangeFn xmasRanges

            -- Determine the function to get the right range given the ratedFor value.

            rangeFn = case ratedFor of
                        X -> _xRange
                        M -> _mRange
                        A -> _aRange
                        S -> _sRange

            -- Set the given range corresponding to the ratedFor value in xmasRanges.

            setIndividualRange :: SingleRange -> XmasRanges
            setIndividualRange newRange
              | ratedFor == X = xmasRanges { _xRange = newRange }
              | ratedFor == M = xmasRanges { _mRange = newRange }
              | ratedFor == A = xmasRanges { _aRange = newRange }
              | ratedFor == S = xmasRanges { _sRange = newRange }
              | otherwise = error (genErrStrForError "Unknown RatedFor value.")

-- This function takes a list of parts associated with the current rule set each one is sitting at,
-- which initially are all the "in" rules set. It processes them one at a time deciding whether they
-- step on to a different rule set or end up in the accept or reject groups. This processing
-- continues recursively until all parts have made their way entirely through the set of rule sets
-- and have reached their destination of being accepted or rejected.

fullyProcessParts :: RuleSetArr -> [PartAndRule] -> AcceptedAndRejected
fullyProcessParts ruleSetArr partStates = (accParts, rejParts)
  where
    accParts = map _part accPartStates
    rejParts = map _part rejPartStates
    (accPartStates, rejPartStates, _) = go ([], [], partStates)

    -- This function iterates until the set of parts to process is empty.

    go :: StateOfParts -> StateOfParts
    go curr@(_, _, []) = curr
    go (acc, rej, (PartAndRule ruleInd part) : others)
      | isNothing newRuleIndMaybe
        = error (genErrStrForError ("No rule match for: " ++ _name ruleToUse))
      | newRuleInd == acceptIndex = go (PartAndRule acceptIndex part : acc, rej, others)
      | newRuleInd == rejectIndex = go (acc, PartAndRule rejectIndex part : rej, others)
      | otherwise = go (acc, rej, PartAndRule newRuleInd part : others)
      where
        newRuleInd = fromJust newRuleIndMaybe
        newRuleIndMaybe = findNewRule (_rules ruleToUse) part
        ruleToUse = ruleSetArr A.! ruleInd

    -- This function will take a part and a rule set and determine which of the rules it matches. It
    -- is able to also return no match, although this should never happen given the data for this
    -- problem.

    findNewRule :: [Rule] -> Part -> Maybe Int
    findNewRule ruleList part = foldr lookForRuleMatch Nothing ruleList
      where
        lookForRuleMatch :: Rule -> Maybe Int -> Maybe Int
        lookForRuleMatch Accept _ = Just acceptIndex
        lookForRuleMatch Reject _ = Just rejectIndex
        lookForRuleMatch (RuleConst (_, ind)) _ = Just ind
        lookForRuleMatch (RuleCond (fn, _, _, _) (_, ind)) acc
          | fn part = Just ind
          | otherwise = acc

-- Given a part, add up the rating numbers of all four parts.

addRatings :: Part -> Int
addRatings (Part x m a s) = x + m + a + s

-- Given the rules and parts read in, create an array of rule sets as well as a map from rule set
-- name to index in this array. Also, include Accept and Reject in array positions 0 and 1 with
-- empty rule sets. This streamlines processing later. We also return the rule and part data
-- structures with added index numbers for the rule set names.

createRuleSetData :: RulesAndParts -> (RulesAndParts, RuleSetArr, RuleSetIndMap)
createRuleSetData rulesAndParts@(RulesAndParts ruleSets _)
  = (rulesAndPartsWithIndexes, ruleSetArr, ruleSetIndMap)
  where
    ruleSetArr = A.listArray arrBounds ruleSetsWithIdNumbers
    ruleSetsWithIdNumbers = rulesForAcceptAndReject ++ _ruleSets rulesAndPartsWithIndexes
    rulesAndPartsWithIndexes = addIdNumbers ruleSetIndMap rulesAndParts
    arrBounds = (0, length justTheNames - 1)
    ruleSetIndMap = M.fromList $ (map (\(i, str) -> (str, i)) . A.assocs) ruleNameArr
    ruleNameArr = A.listArray arrBounds justTheNames
    justTheNames = map _name (rulesForAcceptAndReject ++ ruleSets)
    rulesForAcceptAndReject = [RuleSet "A" acceptIndex [], RuleSet "R" rejectIndex []]
    
    -- Once we have indexes for all of the rule set names, add them to the rule and part data
    -- structures.

    addIdNumbers :: RuleSetIndMap -> RulesAndParts -> RulesAndParts
    addIdNumbers indexMap (RulesAndParts ruleSets' parts) = RulesAndParts newRuleSets parts
      where
        newRuleSets = map addRulePartIds ruleSets'

        addRulePartIds :: RuleSet -> RuleSet
        addRulePartIds (RuleSet name _ rules)
          | isNothing nameIdM = error (genErrStrForError ("No index for: " ++ name))
          | any isNothing newRules = error (genErrStrForError ("No index a rule: " ++ name))
          | otherwise = RuleSet name (fromJust nameIdM) (catMaybes newRules)
          where
            nameIdM  = M.lookup name indexMap
            newRules = map addIdsToRule rules

        addIdsToRule :: Rule -> Maybe Rule
        addIdsToRule Accept = Just Accept
        addIdsToRule Reject = Just Reject
        addIdsToRule (RuleConst (str, _))
          | isNothing nameIdM = error (genErrStrForError ("No index for: " ++ str))
          | otherwise = Just (RuleConst (str, fromJust nameIdM))
          where
            nameIdM = M.lookup str indexMap
        addIdsToRule (RuleCond fnInfo (str, _))
          | isNothing nameIdM = error (genErrStrForError ("No index for: " ++ str))
          | otherwise = Just (RuleCond fnInfo (str, fromJust nameIdM))
          where
            nameIdM = M.lookup str indexMap

-- Functions to parse the input file.

parseInputFile :: Parser RulesAndParts
parseInputFile = do
  ruleSets <- many parseRuleSet
  parts <- many parsePart
  return (RulesAndParts ruleSets parts)

parseRuleSet :: Parser RuleSet
parseRuleSet = do
  name <- identAlpha
  _ <- symbol "{"
  rules <- cslOfRules
  _ <- symbol "}"
  _ <- space
  return (RuleSet name 0 rules)

cslOfRules :: Parser [Rule]
cslOfRules = do
  rule <- parseRule
  rules <- many commaRule
  return (rule : rules)

commaRule :: Parser Rule
commaRule = do
  _ <- symbol ","
  parseRule

parseRule :: Parser Rule
parseRule = do
    (compareFn, rf, ordering, val) <- parseCompareFn
    _ <- symbol ":"
    dest <- identAlpha
    return (RuleCond (compareFn, rf, ordering, val) (dest, 0))
  <|> do
    _ <- symbol "A"
    return Accept
  <|> do
    _ <- symbol "R"
    return Reject
  <|> do
    dest <- identAlpha
    return (RuleConst (dest, 0))

parseCompareFn :: Parser (Part -> Bool, RatedFor, Ordering, Int)
parseCompareFn = do
    _ <- symbol "x<"
    val <- int
    return ((< val) . _x, X, LT, val)
  <|> do
    _ <- symbol "x>"
    val <- int
    return ((> val) . _x, X, GT, val)
  <|> do
    _ <- symbol "m<"
    val <- int
    return ((< val) . _m, M, LT, val)
  <|> do
    _ <- symbol "m>"
    val <- int
    return ((> val) . _m, M, GT, val)
  <|> do
    _ <- symbol "a<"
    val <- int
    return ((< val) . _a, A, LT, val)
  <|> do
    _ <- symbol "a>"
    val <- int
    return ((> val) . _a, A, GT, val)
  <|> do
    _ <- symbol "s<"
    val <- int
    return ((< val) . _s, S, LT, val)
  <|> do
    _ <- symbol "s>"
    val <- int
    return ((> val) . _s, S, GT, val)

parsePart :: Parser Part
parsePart = do
  _ <- symbol "{"
  _ <- symbol "x="
  xVal <- int
  _ <- symbol ","
  _ <- symbol "m="
  mVal <- int
  _ <- symbol ","
  _ <- symbol "a="
  aVal <- int
  _ <- symbol ","
  _ <- symbol "s="
  sVal <- int
  _ <- symbol "}"
  _ <- space
  return (Part xVal mVal aVal sVal)

-- Make error reporting a little simpler.

genErrStrForError :: String -> String
genErrStrForError = genErrStrForErrorFn puzzNum inputFile
