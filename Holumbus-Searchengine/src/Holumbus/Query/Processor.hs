{-# OPTIONS #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Processor
  Copyright  : Copyright (C) 2007, 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable

  The Holumbus query processor. Supports exact word or phrase queries as well
  as fuzzy word and case-insensitive word and phrase queries. Boolean
  operators like AND, OR and NOT are supported. Context specifiers and
  priorities are supported, too.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Query.Processor 
  (
  -- * Processor types
  ProcessConfig (..)
  
  -- * Processing
  , processQuery
  , processPartial
  , processQueryM
  , processPartialM
  )
where

-- import Data.Maybe

import           Control.Monad
import           Control.Parallel.Strategies

import           Data.Binary                    ( Binary (..) )
import           Data.Function
import qualified Data.List                      as L

import           Holumbus.Index.Common          hiding (contexts)
import qualified Holumbus.Index.Common          as IDX

import           Holumbus.Query.Language.Grammar

import           Holumbus.Query.Fuzzy           ( FuzzyScore
                                                , FuzzyConfig
                                                )
import qualified Holumbus.Query.Fuzzy           as F

import           Holumbus.Query.Result          ( Result )

import           Holumbus.Query.Intermediate    ( Intermediate )
import qualified Holumbus.Query.Intermediate    as I

-- ----------------------------------------------------------------------------

-- | The configuration for the query processor.

data ProcessConfig
    = ProcessConfig 
      { fuzzyConfig   :: ! FuzzyConfig -- ^ The configuration for fuzzy queries.
      , optimizeQuery :: ! Bool        -- ^ Optimize the query before processing.
      , wordLimit     :: ! Int         -- ^ The maximum number of words used from a prefix. Zero switches off limiting.
      , docLimit      :: ! Int         -- ^ The maximum number of documents taken into account. Zero switches off limiting.
      }

instance Binary ProcessConfig where
  put (ProcessConfig fc o l d)
      = put fc >> put o >> put l >> put d
  get
      = liftM4 ProcessConfig get get get get

-- | The internal state of the query processor.
data ProcessState i
    = ProcessState 
      { config   :: ! ProcessConfig   -- ^ The configuration for the query processor.
      , contexts :: ! [Context]       -- ^ The current list of contexts.
      , index    :: ! i               -- ^ The index to search.
      , total    :: ! Int             -- ^ The number of documents in the index.
      }

-- ----------------------------------------------------------------------------

-- | Get the fuzzy config out of the process state.

getFuzzyConfig :: HolIndex i => ProcessState i -> FuzzyConfig
getFuzzyConfig = fuzzyConfig . config

-- | Monadic version of 'getFuzzyConfig'.

getFuzzyConfigM :: HolIndexM m i => ProcessState i -> m FuzzyConfig
getFuzzyConfigM s = return $ fuzzyConfig $ config s

-- | Set the current context in the state.

setContexts :: HolIndex i => [Context] -> ProcessState i -> ProcessState i
setContexts cs (ProcessState cfg _ i t) = ProcessState cfg cs i t

-- | Monadic version of 'setContexts'.

setContextsM :: HolIndexM m i => [Context] -> ProcessState i -> m (ProcessState i)
setContextsM cs (ProcessState cfg _ i t) = return $ ProcessState cfg cs i t

-- | Initialize the state of the processor.

initState :: HolIndex i => ProcessConfig -> i -> Int -> ProcessState i
initState cfg i t = ProcessState cfg (IDX.contexts i) i t

-- | Monadic version of 'initState'.

initStateM :: HolIndexM m i => ProcessConfig -> i -> Int -> m (ProcessState i)
initStateM cfg i t = IDX.contextsM i >>= \cs -> return $ ProcessState cfg cs i t

-- | Try to evaluate the query for all contexts in parallel.

forAllContexts :: (Context -> Intermediate) -> [Context] -> Intermediate
forAllContexts f cs = L.foldl' I.union I.emptyIntermediate $ parMap rdeepseq f cs

-- | Monadic version of 'forAllContexts'.

forAllContextsM :: Monad m => (Context -> m Intermediate) -> [Context] -> m Intermediate
forAllContextsM f cs = mapM f cs >>= \is -> return $ L.foldl' I.union I.emptyIntermediate is

-- | Just everything.

allDocuments :: HolIndex i => ProcessState i -> Intermediate
allDocuments s = forAllContexts (\c -> I.fromList "" c $ IDX.allWords (index s) c) (contexts s)

allDocumentsM :: HolIndexM m i => ProcessState i -> m Intermediate
allDocumentsM s = forAllContextsM (\c -> IDX.allWordsM (index s) c >>= \r -> return $ I.fromList "" c r) (contexts s)

-- | Process a query only partially in terms of a distributed index. Only the intermediate 
-- result will be returned.

processPartial :: (HolIndex i) => ProcessConfig -> i -> Int -> Query -> Intermediate
processPartial cfg i t q = process (initState cfg i t) oq
  where
  oq = if optimizeQuery cfg then optimize q else q

-- | Monadic version of 'processPartial'.

processPartialM :: (HolIndexM m i) => ProcessConfig -> i -> Int -> Query -> m Intermediate
processPartialM cfg i t q = initStateM cfg i t >>= (flip processM) oq
  where
  oq = if optimizeQuery cfg then optimize q else q

-- | Process a query on a specific index with regard to the configuration.

processQuery :: (HolIndex i, HolDocuments d c) => ProcessConfig -> i -> d c -> Query -> Result c
processQuery cfg i d q = I.toResult d (processPartial cfg i (sizeDocs d) q)

-- | Monadic version of 'processQuery'.

processQueryM :: (HolIndexM m i, HolDocuments d c) => ProcessConfig -> i -> d c -> Query -> m (Result c)
processQueryM cfg i d q = processPartialM cfg i (sizeDocs d) q >>= \ir -> return $ I.toResult d ir

-- | Continue processing a query by deciding what to do depending on the current query element.

process :: HolIndex i => ProcessState i -> Query -> Intermediate
process s (Word w)           = processWord s w
process s (Phrase w)         = processPhrase s w
process s (CaseWord w)       = processCaseWord s w
process s (CasePhrase w)     = processCasePhrase s w
process s (FuzzyWord w)      = processFuzzyWord s w
process s (Negation q)       = processNegation s (process s q)
process s (Specifier c q)    = process (setContexts c s) q
process s (BinQuery o q1 q2) = processBin o (process s q1) (process s q2)

-- | Monadic version of 'process'.

processM :: HolIndexM m i => ProcessState i -> Query -> m Intermediate
processM s (Word w)           = processWordM s w
processM _ (Phrase _)         = return I.emptyIntermediate -- processPhraseM s w
processM s (CaseWord w)       = processCaseWordM s w
processM _ (CasePhrase _)     = return I.emptyIntermediate -- processCasePhraseM s w
processM s (FuzzyWord w)      = processFuzzyWordM s w
processM s (Negation q)       = processM s q >>= processNegationM s
processM s (Specifier c q)    = setContextsM c s >>= \ns -> processM ns q
processM s (BinQuery o q1 q2) = do
  ir1 <- processM s q1
  ir2 <- processM s q2
  return $ processBin o ir1 ir2

-- | Process a single, case-insensitive word by finding all documents whreturn I.emptyIntermediate -- ich contain the word as prefix.

processWord :: HolIndex i => ProcessState i -> String -> Intermediate
processWord s q = forAllContexts wordNoCase (contexts s)
  where
  wordNoCase c = I.fromList q c $ limitWords s $ IDX.prefixNoCase (index s) c q

-- | Monadic version of 'processWord'.

processWordM :: HolIndexM m i => ProcessState i -> String -> m Intermediate
processWordM s q = forAllContextsM wordNoCase (contexts s)
  where
  wordNoCase c = IDX.prefixNoCaseM (index s) c q >>= limitWordsM s >>= \r -> return $ I.fromList q c r

-- | Process a single, case-sensitive word by finding all documents which contain the word as prefix.

processCaseWord :: HolIndex i => ProcessState i -> String -> Intermediate
processCaseWord s q = forAllContexts wordCase (contexts s)
  where
  wordCase c = I.fromList q c $ limitWords s $ IDX.prefixCase (index s) c q

-- | Monadic version of 'processCaseWord'.

processCaseWordM :: HolIndexM m i => ProcessState i -> String -> m Intermediate
processCaseWordM s q = forAllContextsM wordCase (contexts s)
  where
  wordCase c = IDX.prefixCaseM (index s) c q >>= limitWordsM s >>= \r -> return $ I.fromList q c r

-- | Process a phrase case-insensitive.

processPhrase :: HolIndex i => ProcessState i -> String -> Intermediate
processPhrase s q = forAllContexts phraseNoCase (contexts s)
  where
  phraseNoCase c = processPhraseInternal (IDX.lookupNoCase (index s) c) c q

-- processPhraseM :: HolIndexM m i => ProcessState i -> String -> m Intermediate
-- processPhraseM s q = forAllContextsM phraseNoCase (contexts s)
--   where
--   phraseNoCase c = 

-- | Process a phrase case-sensitive.

processCasePhrase :: HolIndex i => ProcessState i -> String -> Intermediate
processCasePhrase s q = forAllContexts phraseCase (contexts s)
  where
  phraseCase c = processPhraseInternal (IDX.lookupCase (index s) c) c q

-- | Process a phrase query by searching for every word of the phrase and comparing their positions.

processPhraseInternal :: (String -> RawResult) -> Context -> String -> Intermediate
processPhraseInternal f c q = let
  w = words q 
  m = mergeOccurrencesList $ map snd $ f (head w) in
  if nullDocIdMap m
  then I.emptyIntermediate
  else I.fromList q c [(q, processPhrase' (tail w) 1 m)]
  where
  processPhrase' :: [String] -> Position -> Occurrences -> Occurrences
  processPhrase' [] _ o = o
  processPhrase' (x:xs) p o = processPhrase' xs (p+1) (filterWithKeyDocIdMap (nextWord $ map snd $ f x) o)
    where
      nextWord :: [Occurrences] -> DocId -> Positions -> Bool
      nextWord [] _ _  = False
      nextWord no d np = maybe False hasSuccessor (lookupDocIdMap d (mergeOccurrencesList no))
          where
            hasSuccessor :: Positions -> Bool
            hasSuccessor w = foldPos (\cp r -> r || (memberPos (cp + p) w)) False np

-- | Process a single word and try some fuzzy alternatives if nothing was found.

processFuzzyWord :: HolIndex i => ProcessState i -> String -> Intermediate
processFuzzyWord s oq = processFuzzyWord' (F.toList $ F.fuzz (getFuzzyConfig s) oq) (processWord s oq)
  where
  processFuzzyWord' :: [(String, FuzzyScore)] -> Intermediate -> Intermediate
  processFuzzyWord' []     r = r
  processFuzzyWord' (q:qs) r = if I.null r then processFuzzyWord' qs (processWord s (fst q)) else r

-- | Monadic version of 'processFuzzyWord'.

processFuzzyWordM :: HolIndexM m i => ProcessState i -> String -> m Intermediate
processFuzzyWordM s oq = do
  sr <- processWordM s oq 
  cfg <- getFuzzyConfigM s
  processFuzzyWordM' (F.toList $ F.fuzz cfg oq) sr
    where
    processFuzzyWordM' []     r = return r
    processFuzzyWordM' (q:qs) r = if I.null r
                                  then processWordM s (fst q) >>= processFuzzyWordM' qs
                                  else return r

-- | Process a negation by getting all documents and substracting the result of the negated query.

processNegation :: HolIndex i => ProcessState i -> Intermediate -> Intermediate
processNegation s r = I.difference (allDocuments s) r

-- | Monadic version of 'processNegation'.

processNegationM :: HolIndexM m i => ProcessState i -> Intermediate -> m Intermediate
processNegationM s r1 = allDocumentsM s >>= \r2 -> return $ I.difference r2 r1

-- | Process a binary operator by caculating the union or the intersection of the two subqueries.

processBin :: BinOp -> Intermediate -> Intermediate -> Intermediate
processBin And r1 r2 = I.intersection r1 r2
processBin Or r1 r2  = I.union r1 r2
processBin But r1 r2 = I.difference r1 r2


-- | Limit a 'RawResult' to a fixed amount of the best words.
--
-- First heuristic applied is limiting the number of documents in the result,
-- assuming the short words come first in the result list
-- So the length of the result list depends on the number of documents found.
--
-- TODO: This is fixed to 2000, should be part of the config part of the state
--
-- A 2. simple heuristic is used to 
-- determine the quality of a word: The total number of occurrences divided by the number of 
-- documents in which the word appears.
--
-- The second heuristic isn't that expensive any more when the resul list is cut of by the heuristic
--
-- The limit 500 should be part of a configuration

limitWords              :: ProcessState i -> RawResult -> RawResult
limitWords s r          = cutW . cutD $ r
  where
  limitD                = docLimit $ config s
  cutD
      | limitD > 0      = limitDocs limitD
      | otherwise       = id

  limitW                = wordLimit $ config s
  cutW
      | limitW > 0
        &&
        length r > limitW
                        = map snd . take limitW . L.sortBy (compare `on` fst) . map calcScore
      | otherwise       = id

  calcScore             :: (Word, Occurrences) -> (Double, (Word, Occurrences))
  calcScore w@(_, o)    = (log (fromIntegral (total s) / fromIntegral (sizeDocIdMap o)), w)

-- ----------------------------------------------------------------------------

-- | Limit the number of docs in a raw result

limitDocs               :: Int -> RawResult -> RawResult
limitDocs _     []      = []
limitDocs limit _
    | limit <= 0        = []
limitDocs limit (x:xs)  = x : limitDocs (limit - sizeDocIdMap (snd x)) xs

-- ----------------------------------------------------------------------------

-- | Monadic version of 'limitWords'.
limitWordsM             :: (Monad m) => ProcessState i -> RawResult -> m RawResult
limitWordsM s r         = return $ limitWords s r

-- | Merge occurrences
mergeOccurrencesList    :: [Occurrences] -> Occurrences
mergeOccurrencesList    = unionsWithDocIdMap unionPos

-- ----------------------------------------------------------------------------
