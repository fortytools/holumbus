{-# OPTIONS #-}

-- ------------------------------------------------------------

module Main
where

import           Control.Parallel.Strategies

import           Data.Function.Selector

import           Data.Binary			( Binary )
import qualified Data.Binary			as B			-- else naming conflict with put and get from Monad.State
import           Data.Char
import           Data.List
import		 Data.Maybe
import qualified Data.Map       		as M

import		 Holumbus.Crawler.Core
import           Holumbus.Crawler.Html
import           Holumbus.Crawler.Util

import           System.IO
import		 System.Environment

import		 Text.XML.HXT.Arrow		hiding ( when
						       , getState
						       )
import qualified Text.XML.HXT.Arrow		as X

import           Text.Regex.XMLSchema.String

-- import qualified Debug.Trace as D

-- ------------------------------------------------------------

getOptions		:: [String] -> (Maybe String, String)
getOptions ("-r":fn:as)	= (Just fn, r)
			  where
			  (_, r) = getOptions as
getOptions (out:_)	= (Nothing, out)
getOptions []		= (Nothing, "")

main	:: IO ()
main	= do
	  (resume, out) <- getArgs >>= return . getOptions
	  let action	= maybe (crawlDocs ["http://www.fussballersprueche.de/"])
				crawlerResume
				$
				resume
	  (_, docs) <- runCrawler action
                                  testCrawlerConfig
				  jokeCrawlerInitState
	  writeFile out $ showJokesAsHaskell (getS theResultAccu $ docs)
	  -- writeFile $ showJokesAsHaskell res
{-
	  runX ( constA (getS theResultAccu $ docs)
		 >>>
		 xpickleVal (xpTextDocs "http://localhost/~si/")
		 >>>
		 addXHtmlDoctypeTransitional
		 >>>
		 writeDocument [ (a_indent, v_1)
			       , (a_output_encoding, usAscii)
			       , (a_output_html, v_1)
			       ] out
	       )
-}
	  return ()

-- ------------------------------------------------------------

data JComp              = Jid | Jauthor | Jclass | Jtext | Jcontext
			   deriving (Eq, Show)

type Jokes		= [Joke]

data Joke               = Joke { jid 	 :: ! Int
			       , jwho   :: ! String
			       , jwhat  :: ! String
			       , jwhere :: ! String
			       , jclass :: ! String
			       }
			  deriving Show


instance NFData Joke where
    rnf (Joke _x1 x2 x3 x4 x5)
			= rnf x2 `seq` rnf x3 `seq` rnf x4 `seq` rnf x5

instance Binary Joke where
    put (Joke x1 x2 x3 x4 x5)
			= B.put x1 >> B.put x2 >> B.put x3 >> B.put x4 >> B.put x5
    get			= do
			  x1 <- B.get
			  x2 <- B.get
			  x3 <- B.get
			  x4 <- B.get
			  x5 <- B.get
			  return $ Joke x1 x2 x3 x4 x5

mkJokes			:: [(JComp, String)] -> Jokes

mkJokes ((Jauthor, a) : (Jid, i) : (Jclass, c) : (Jtext, t) : (Jcontext, x) : rest)
			= Joke { jid = read i, jwho = a, jwhat = trim 1 t, jwhere = trim 1 x, jclass = c } : mkJokes rest
			  where
			  trim n               = drop n . reverse . drop n . reverse

mkJokes ((Jauthor, a) : (Jid, i) : (Jclass, c) : (Jtext, t) : rest)
			= mkJokes $ (Jauthor, a) : (Jid, i) : (Jclass, c) : (Jtext, t) : (Jcontext, "") : rest

mkJokes ((Jauthor, a) : (Jid, i) : rest)
    			= mkJokes $ (Jauthor, a) : (Jid, i) : (Jclass, "") : rest

mkJokes ((Jid, i) : rest)
			= mkJokes $ (Jauthor, "") : (Jid, i) : rest

mkJokes (_:xs)		= mkJokes xs

mkJokes []		= []

-- ------------------------------------------------------------

patchURI	:: String -> Maybe String
patchURI u	= case matchURI u of
		  [("uri", u'),("page", p')] -> Just $ u' ++ "&katID=new&" ++ p'
		  _	-> Nothing
    where
    matchURI	= matchSubex "({uri}.*[?]action=show).*katID=new.*({page}page=[0-9]+)&.*"

-- ------------------------------------------------------------

type JokeTable		= M.Map Int Joke

type TextCrawlerConfig	= CrawlerConfig Jokes JokeTable

emptyJokeTable		:: JokeTable
emptyJokeTable		= M.empty

jokeCrawlerConfig	:: TextCrawlerConfig
jokeCrawlerConfig	= addReadAttributes  [ ]				-- at the moment no more read attributes are neccessary
			  >>>
			  setS theFollowRef 	followRefs
			  >>>
			  setS theProcessRefs   ( getHtmlReferences >>> arrL (maybeToList . patchURI) )
			  >>>
			  setS thePreDocFilter	documentOK
			  >>>
			  setS theProcessDoc	collectJokes
			  $
			  baseConfig
    where
    baseConfig 		= defaultHtmlCrawlerConfig insertJokes			-- take the default HTML crawler config
										-- and set the accumulator op
    insertJokes		:: AccumulateDocResult Jokes JokeTable
    insertJokes x	= return . uncurry insertJ x
			  where
			  insertJ _uri js jt = foldl' insertJ' jt $ js
			                       where
					       insertJ' jt' j' = M.insert (jid j') j' jt'

    collectJokes	= rnfA $						-- force complete evaluation of the result: this is essential, don't delete rnfA
			  fromLA $
			  listA theJokes >>^ {- (\ x -> D.trace (show x) x ) >>^ -} mkJokes

    followRefs		= const True						-- all hrefs are collected

    documentOK		= ( getAttrValue transferStatus >>> isA (== "200") )	-- document transfer status must be 200 OK
			  `guards`
			  this

    theJokes		:: LA XmlTree (JComp, String)
    theJokes		= ( multi
			    $
			    ( ( hasName "tr"
				>>>
				has ( this
				      /> hasName "td"
				      /> (hasName "div" >>> hasAttrValue "class" (== "showName"))
				      /> (hasName "a"   >>> hasAttrValue "class" (== "showName"))
				    )
			      )
			      <+>
			      ( hasName "td"
				>>> hasAttrValue "id" ("spruch" `isPrefixOf`) )
			    )
			  )
			  >>>
			  ( ( this
			      /> hasName "td"
			      /> ( hasName "div"
				   >>> hasAttrValue "class" (== "showName")
				 )
			      />
			      ( ( hasName "a"
				  >>> getAttrValue0 "name"
				  >>^ (\ i -> (Jid, i)) )
				<+>
				( hasName "a"
				  >>> hasAttrValue "class" (== "showName")
				  />  getText
				  >>^ (\ t -> (Jauthor, t)))
			      )
			    )
			    <+>
			    ( this
			      />  hasName "td"
			      />  hasName "table"
			      //> hasName "b"
			      />  ( hasName "a"
				    >>> hasAttrValue "class" (=="middle")
				  )
			      />  hasName "b"
			      />  isText >>> getText >>^ (\ c -> (Jclass, c))
			    )
			    <+>
			    ( hasAttrValue "id" ("spruch" `isPrefixOf`)
			      /> hasName "div"
			      />( ( hasText (any (not . isSpace))
				    >>> getText
				    >>^ (\ t -> (Jtext, t))
				  )
				  <+>
				  ( hasName "small"
				    /> getText
				    >>^ (\ c -> (Jcontext, c))
				  )
				)
			    )
			  )

    has t                = deep t `guards` this

-- ------------------------------------------------------------

jokeCrawlerInitState	:: CrawlerState JokeTable
jokeCrawlerInitState	= initCrawlerState emptyJokeTable

-- ------------------------------------------------------------

testCrawlerConfig 	:: CrawlerConfig Jokes JokeTable
testCrawlerConfig	= setS theFollowRef
			  ( simpleFollowRef'
			    [ "http://www.fussballersprueche.de/index.php[?].*&katID=new&page=[0-9]+.*"
			    ]
			    [ -- ".*"
			    ]
			  )
			  >>>
			  setS theMaxNoOfDocs 200						-- limit of docs to be crawled
			  >>>
			  setS theSaveIntervall 10						-- every 20 documents the state is saved
			  >>>
			  setS theSavePathPrefix "./tmp/fussballsprueche-"			-- states are saved in subdir "./tmp" in files starting with "hc-"
			  >>>
			  setS theTraceLevel 1							-- trace actions with lowest level
			  $
			  jokeCrawlerConfig

-- ------------------------------------------------------------

showJokesAsHaskell	:: JokeTable -> String
showJokesAsHaskell jt	= unlines
			  [ "module FussballerSprueche"
			  , "where"
			  , ""
			  , "type Joke = (Int, String, String, String, String)"
			  , ""
			  , "allJokes :: [Joke]"
			  , "allJokes = [" ++ intercalate ", " ( map (("joke" ++) . show) . M.keys $ jt ) ++ "]"
			  , ""
			  ]
			  ++ M.fold ((++) . showJokeAsHaskell) "" jt
			  ++ "\n\n"

showJokeAsHaskell	:: Joke -> String
showJokeAsHaskell (Joke x1 x2 x3 x4 x5)
                     	= unlines $
			  [ unwords ["joke" ++ show x1, ":: Joke"]
			  , unwords ["joke" ++ show x1, "= (", show x1, "," , show x2, ",", show x3, ",", show x4, ",", show x5, ")"]
			  , ""
			  ]
			  
-- ------------------------------------------------------------
