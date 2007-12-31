-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Common
  Copyright  : Copyright (C) 2007 Sebastian M. Schlatt, Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Common data types shared by all index types and a unified interface for
  all different index types.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Common where

import Text.XML.HXT.Arrow.Pickle  -- nice pickling stuff

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

-- | The table which is used to map a document to an artificial id and vice versa.
data Documents     = DocTable { idToDoc   :: !(IntMap Document)
                              , docToId   :: !(Map URL DocId) 
                              , lastDocId :: !DocId
                              } deriving (Show)

-- | A document consists of a title and it's unique identifier.
type Document      = (Title, URL)

type DocId         = Int
type URL           = String
type Title         = String

type Position      = Int
type Context       = String
type Word          = String

-- | The occurrences in a number of documents. A mapping from document ids to the positions in the document.
type Occurrences   = IntMap Positions
-- | The positions of the word in the document.
type Positions     = IntSet

-- | This class provides a generic interface to different types of index implementations.
class HolIndex i where
  -- | Create an empty index.
  empty         :: i

  -- | Returns the number of unique documents in the index.
  sizeDocs      :: i -> Int
  -- | Returns the number of unique words in the index.
  sizeWords     :: i -> Int
  -- | Returns the table for mapping between documents and their ids.
  documents     :: i -> Documents
  -- | Returns a list of all contexts avaliable in the index.
  contexts      :: i -> [ Context ]

  -- | Returns the occurrences for every word. A potentially expensive operation.
  allWords      :: Context -> i -> [(String, Occurrences)]
  -- | Searches for words beginning with the prefix in a given context (case-sensitive).
  prefixCase    :: Context -> i -> String -> [(String, Occurrences)]
  -- | Searches for words beginning with the prefix in a given context (case-insensitive).
  prefixNoCase  :: Context -> i -> String -> [(String, Occurrences)]
  -- | Searches for and exact word in a given context (case-sensitive).
  lookupCase    :: Context -> i -> String -> [Occurrences]
  -- | Searches for and exact word in a given context (case-insensitive).
  lookupNoCase  :: Context -> i -> String -> [Occurrences]

  -- | Inserts an occurrence of a word for a given context.
  insert        :: Context -> Word -> Position -> Document -> i -> i
  -- | Updates an occurrence of a word for a given context.
  update        :: Context -> Word -> Position -> Document -> i -> i

  -- | Load Index from XML file
  loadFromFile :: String -> IO[i]


-- | Create an empty table.
emptyDocuments :: Documents
emptyDocuments = DocTable IM.empty M.empty 0

-- | Create an empty set of positions.
emptyOccurrences :: Occurrences
emptyOccurrences = IM.empty



--------------------------------------------------------------------------------
-- i think this should be in the common module since any other place would lead
-- to code duplication or further modules

instance XmlPickler Documents where
   xpickle =  xpWrap  ( \itd -> DocTable itd (itd2dti itd) 100
                      , \(DocTable itd _ _) -> itd
		              )
--		              (xpTriple
		                  (xpWrap (IM.fromList, IM.toList)
		                  		(xpList (xpElem "doc" (xpPair 
		                  				(xpAttr "id" xpPrim)
		                  				(xpPair 
		                  					(xpAttr "href" xpText)
		                  					(xpAttr "title" xpText)
		                  				)
		                  		)))
	--	                  ) xpZero xpZero
		              )
	where
		itd2dti :: IntMap Document -> Map URL DocId
		itd2dti = IM.foldWithKey (\i (_, s2) r -> M.insert s2 i r) M.empty
			              	
		              

