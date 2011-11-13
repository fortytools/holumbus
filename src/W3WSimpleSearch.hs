-- ----------------------------------------------------------------------------

{- |
  A simple example of searching with Holumbus, providing a command line search with the
  default query language.

-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-type-defaults  #-}

module W3WSimpleSearch where

import           Control.Monad          ( when )
import           Data.Function
import qualified Data.List              as L
import qualified Data.Map               as M
import           Data.Maybe
import qualified Data.IntMap            as IM
import           Data.String.Unicode    ( utf8ToUnicode )
import           Holumbus.Index.Common
import           Holumbus.Query.Language.Grammar
import           Holumbus.Query.Language.Parser
import           Holumbus.Query.Processor
import           Holumbus.Query.Result
import           Holumbus.Query.Ranking
import           Holumbus.Query.Fuzzy
import           System.IO
import           System.Environment
import           System.Exit
import           System.Console.GetOpt
import           System.CPUTime
import           Text.Printf
import           IndexTypes
import 			     Control.Monad.Trans
import 			     PageInfo

-- ------------------------------------------------------------
-- Representation of all Document-Hits and Word-Completions found
data SearchResult = SearchResult
  { srDocs		:: SearchResultDocs
  ,	srWords		:: SearchResultWords
  }

data SearchResultDocs = SearchResultDocs
  { srTime      :: Float
  ,	srDocCount  :: Int
  ,	srDocHits   :: [SRDocHit]
  } deriving Show

data SearchResultWords = SearchResultWords
  { srWordCount :: Int
  ,	srWordHits  :: [SRWordHit]
  }

data SRDocHit = SRDocHit
  { srTitle  :: String
  , srScore  :: Float
  , srPageInfo :: PageInfo
  , srUri    :: String
  , srContextMap  :: M.Map Context DocWordHits -- Context: "title", "keywords", "content", "dates", ...
                                               -- DocWordHits: Map Word Positions
  } deriving Show

data SRWordHit = SRWordHit
  { srWord :: String
  ,	srHits :: Int
  }

-- Ranking for different kinds of Contexts
type RankTable  = [(Context, Score)]

defaultRankTable :: RankTable
defaultRankTable
    = [ ("title", 1.0)
      , ("headlines", 1.0)
      , ("contentContext", 0.5)
      , ("uri", 0.1)
      , ("datesContext", 1.0)
      , ("calenderContext", 2.0)
      ]

defaultRankCfg :: RankConfig a
defaultRankCfg
    = RankConfig
      (docRankWeightedByCount  defaultRankTable)
      (wordRankWeightedByCount defaultRankTable)

-- ------------------------------------------------------------

-- | Just an alias with explicit type.
loadIndex       :: FilePath -> IO CompactInverted
loadIndex       = loadFromFile

-- | Just an alias with explicit type.
loadDocuments   :: FilePath -> IO (SmallDocuments PageInfo)
loadDocuments   = loadFromFile

-- ------------------------------------------------------------

-- | Create the configuration for the query processor.
processCfg :: ProcessConfig
processCfg = ProcessConfig (FuzzyConfig True True 1.0 germanReplacements) True 100 500

-- | Perform a query on a local index.
localQuery              :: CompactInverted -> SmallDocuments PageInfo -> (Query -> IO (Result PageInfo))
localQuery idx doc q    = return (processQuery processCfg idx doc q)

-- | get all Search Results
getAllSearchResults :: String -> (Query -> IO (Result PageInfo)) -> IO SearchResult
getAllSearchResults q f = do
	docs <- getIndexSearchResults q f
	words <- getWordCompletions q f
	return $ SearchResult docs words

-- | insert time needed for request into SearchResultDocs data type
mkDocSearchResult :: Float -> SearchResultDocs -> SearchResultDocs
mkDocSearchResult requestTime searchResultDocs = SearchResultDocs requestTime (srDocCount searchResultDocs) (srDocHits searchResultDocs)

-- | get only Document Search Results (without Word-Completions)
getIndexSearchResults :: String -> (Query -> IO (Result PageInfo)) -> IO SearchResultDocs
getIndexSearchResults q f =
	answerThis q
	where
	answerThis q = do
		result <- either printError makeQuery pr
		return result
			where
			pr = parseQuery q
			printError err = do
				return $ SearchResultDocs 0.0 0 []
			makeQuery pq = do
				t1 <- getCPUTime
				r <- f pq -- This is where the magic happens!
				rr <- return (rank defaultRankCfg r)
				docsSearchResult <- getDocHits (docHits rr)
				t2 <- getCPUTime
				d <- return ((fromIntegral (t2 - t1) / 1000000000000.0) :: Float)
				return $ mkDocSearchResult d docsSearchResult

-- | get only Word-Completions (without Document Search Results)
getWordCompletions :: String -> (Query -> IO (Result PageInfo)) -> IO SearchResultWords
getWordCompletions q f =
	answerThis q
	where
	answerThis q = do
		result <- either printError makeQuery pr
		return result
			where
			pr = parseQuery q
			printError err = do
				return $ SearchResultWords 0 []
			makeQuery pq = do
				r <- f pq -- This is where the magic happens!
				rr <- return (rank defaultRankCfg r)
				getWordHits (wordHits rr)

-- | convert Document-Hits to SearchResult
getDocHits :: DocHits PageInfo -> IO SearchResultDocs
getDocHits h = do
	return $ SearchResultDocs 0.0 (IM.size h) (map docInfoToSRDocHit docData)
	where
		docData = (L.reverse $ L.sortBy (compare `on` (docScore . fst . snd)) $ IM.toList h)

docInfoToSRDocHit :: (DocId, (DocInfo PageInfo, DocContextHits)) -> SRDocHit
docInfoToSRDocHit (_, ((DocInfo (Document title uri Nothing) score), contextMap)) = SRDocHit title score emptyPageInfo uri contextMap
docInfoToSRDocHit (_, ((DocInfo (Document title uri (Just pageInfo)) score), contextMap)) = SRDocHit title score pageInfo uri contextMap


-- | convert Word-Completions to SearchResult
getWordHits :: WordHits -> IO SearchResultWords
getWordHits h = do
	return $ SearchResultWords (M.size h) (getWordHits' wordData)
	where
		wordData = (L.reverse $ L.sortBy (compare `on` snd) (map (\(c, (_, o)) -> (c, M.fold (\m r -> r + IM.size m) 0 o)) (M.toList h)))
		getWordHits' [] = []
		getWordHits' ((c,s):xs) =
			[ SRWordHit c s ]
			++ (getWordHits' xs)

