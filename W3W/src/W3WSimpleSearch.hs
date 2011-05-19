-- ----------------------------------------------------------------------------

{- |
  A simple example of searching with Holumbus, providing a command line search with the
  default query language.

-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-type-defaults  #-}

module Main where

import           Control.Exception
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
import           System.Console.Haskeline
import           System.Console.Haskeline.IO
import           System.Console.GetOpt
import           System.CPUTime

import           Text.Printf

import           W3W.IndexTypes

-- ------------------------------------------------------------

data Flag = Index String 
          | Documents String 
          | Verbose 
          | Version 
          | Help deriving (Show, Eq)

isIndex                 :: Flag -> Bool
isIndex (Index _)       = True
isIndex _               = False

isDocuments             :: Flag -> Bool
isDocuments (Documents _)
                         = True
isDocuments _           = False

-- ------------------------------------------------------------

type RankTable  = [(Context, Score)]

defaultRankTable :: RankTable
defaultRankTable
    = [ ("title", 0.8)
      , ("keywords", 0.6)
      , ("headlines", 0.4)
      , ("content", 0.2)
      , ("uri", 0.1)
      , ("uriclass", 0.1)
      ]

defaultRankCfg :: RankConfig a
defaultRankCfg
    = RankConfig
      (docRankWeightedByCount  defaultRankTable)
      (wordRankWeightedByCount defaultRankTable)

-- ------------------------------------------------------------

version :: String
version = "0.1"

main :: IO ()
main = 
  do
  argv  <- getArgs
  flags <- commandLineOpts argv

  when (Version `elem` flags) $
       putStrLn version
       >> exitWith ExitSuccess

  when (Help `elem` flags) $
       usage []
       >> exitWith ExitSuccess

  let verbose  = Verbose `elem` flags
  let doc      = filter isDocuments flags

  when (L.null doc) $
       usage ["No documents file given!\n"]
  when (length doc > 1) $
       usage ["Only one documents file allowed!\n"]

  let idx = filter isIndex flags

  when (L.null idx) $
       usage ["No index file given!\n"]
  when (length idx > 1) $
       usage ["Only one index file allowed!\n"]

  startupLocal verbose (head idx) (head doc)

-- ------------------------------------------------------------

verb    :: Bool -> IO () -> IO ()
verb    = when

-- | Startup using local index.
startupLocal :: Bool -> Flag -> Flag -> IO ()
startupLocal v (Index idxFile) (Documents docFile) = 
  do
  verb v $ putStrLn "Loading index..."
  idx <- loadIndex idxFile
  verb v $ putStrLn ("Loaded " ++ show (sizeWords idx) ++ " words")
  verb v $ putStrLn "Loading documents..."
  doc <- loadDocuments docFile
  verb v $ putStrLn ("Loaded " ++ show (sizeDocs doc) ++ " documents ")
  bracketOnError
    (initializeInput defaultSettings)
    cancelInput
    (\hd -> (answerQueries hd idx doc v $ localQuery idx doc)
            >>
            closeInput hd
    )

startupLocal _ _ _ = usage ["Internal error!\n"]

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
localQuery              :: CompactInverted -> SmallDocuments PageInfo -> Query -> IO (Result PageInfo)
localQuery idx doc q    = return (processQuery processCfg idx doc q)

-- ------------------------------------------------------------

usage :: [String] -> IO a
usage errs
    | L.null errs       = do
                          hPutStrLn stdout use
                          exitWith ExitSuccess
    | otherwise         = do
                          hPutStrLn stderr (concat errs ++ "\n" ++ use)
                          exitWith (ExitFailure (-1))
    where
    header = "SimpleSearch - A simple command-line search using the Holumbus library.\n\n" ++
             "Usage: SimpleSearch [OPTIONS]"
    use    = usageInfo header options

commandLineOpts :: [String] -> IO [Flag]
commandLineOpts argv = 
  case getOpt Permute options argv of
  (o, [], []  ) -> return o
  (_, _, errs) -> usage errs

options :: [OptDescr Flag]
options = [ Option "i" ["index"] (ReqArg Index "FILE") "Loads index from FILE"
          , Option "d" ["documents"] (ReqArg Documents "FILE") "Loads documents from FILE"
          , Option "v" ["verbose"] (NoArg Verbose) "Be more verbose"
          , Option "V" ["version"] (NoArg Version) "Output version and exit"
          , Option "?" ["help"] (NoArg Help) "Output this help and exit"
          ]

-- ------------------------------------------------------------

answerQueries                      :: InputState ->
                                      CompactInverted ->
                                      SmallDocuments PageInfo ->
                                      Bool ->
                                          (Query -> IO (Result a)) -> IO ()
answerQueries hd idx doc verbose f = 
    do
    q <- queryInput hd (getInputLine "Enter query (type :? for help) > ")
    when (isJust q)
         (do
           let n = fst $ utf8ToUnicode (fromJust q)
           answerThis n
         )
    answerMore
    where
    verb'
        = when verbose

    internal ""
        = return ()
    internal "?"
        = do
          putStrLn ""
          printHelp
    internal "q"
        = exitWith ExitSuccess
    internal "c"
        = putStrLn $ "contexts: " ++ show (contexts idx)
    internal ('w' : ' ' : cx)
        = putStrLn $ "words in " ++ show cx ++ ": " ++ (show . map fst . allWords idx $ cx)
    internal q
        = putStrLn $ "unknown command: :" ++ q

    answerMore
        = answerQueries hd idx doc verbose f

    answerThis ""
        = return ()
    answerThis (':' : q)
        = do
          internal q
    answerThis q
        = do
          verb' $ putStrLn ("query: \n" ++ (show pr) ++ "\n")
          either printError makeQuery pr
        where
        pr = parseQuery q
        printError err = putStrLn ("problem parsing query: " ++ err)
        makeQuery pq
            = do
              t1 <- getCPUTime
              r <- f pq -- This is where the magic happens!
              rr <- return (rank defaultRankCfg r)

              printDocHits (docHits rr)
              putStrLn ""
              printWordHits (wordHits rr)
              t2 <-  getCPUTime
            
              d <- return ((fromIntegral (t2 - t1) / 1000000000000) :: Float)

              ds <- return (printf "%.4f" d)
              putStrLn ""
              putStrLn ("query processed in " ++ ds ++ " sec")

printDocHits :: DocHits a -> IO ()
printDocHits h = 
  do
  putStrLn "Result:"
  printHits' (L.sortBy (compare `on` (docScore . fst . snd)) $ IM.toList h)
  putStrLn ""
  putStrLn ("Found " ++ (show (IM.size h)) ++ " documents")
    where
      printHits' [] = return ()
      printHits' ((_, (di, _)):xs) = 
        do
        putStr (title $ document di)
        putStr " Score: "
        putStrLn (show $ docScore  di)
        putStrLn (uri $ document di)
        printHits' xs
        return ()

printWordHits :: WordHits -> IO ()
printWordHits h = 
  do
  putStrLn "Completions:"
  d <- return (L.sortBy (compare `on` snd) (map (\(c, (_, o)) -> (c, M.fold (\m r -> r + IM.size m) 0 o)) (M.toList h)))
  putStrLn (foldr (\(c, s) r -> r ++ c ++ " (" ++ (show s) ++ ") ") "" d)
  putStrLn ""
  putStrLn ("Found " ++ (show (M.size h)) ++ " possible completions")

printHelp :: IO ()
printHelp = 
  sequence_ $
  map putStrLn $
  [ "Holumbus treats single words as prefix terms and will give you possible completions."
  , "Words are interpreted case insensitive. Phrases and exact matches (case-sensitive)"
  , "can be specified by using quotes (i.e. \"Foo Bar\" will match this exact sequence)."
  , "Terms just separated by space will be treated implicitly as AND terms."
  , "Other operators have to be specified explicitly. Avaliable operators are: AND, OR, NOT"
  , "Priority can be influenced by round parantheses. If unsure about spelling, a single"
  , "word can be preceeded by ~ to make a fuzzy query."
  , "The contexts to search can be restricted with the : operator (seperate them with , )."
  , "Example: firstcontext,secondcontext:(foo OR bar) NOT foobar"
  , "This will search for documents containing \"foo\" or \"bar\" in the contexts named"
  , "\"firstcontext\" and \"secondcontext\" and no \"foobar\" in the all contexts."
  , ""
  , "commands:"
  , ":q          exit"
  , ":?           help"
  , ":c           list all contexts"
  , ":w <context> list all words in a context."
  ]
