-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Inverted
  Copyright  : Copyright (C) 2007 Sebastian M. Schlatt, Timo B. Huebel
  License    : MIT
  
  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
  The inverted index for Holumbus.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Inverted where

import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.IntMap as IM

import Holumbus.Data.StrMap (StrMap)
import qualified Holumbus.Data.StrMap as SM

import Holumbus.Index.Common

import Text.XML.HXT.Arrow   			-- Import stuff for pickling
--import Data.IntSet                     	-- can probably be removed later on
import qualified Data.IntSet as IS


-- | The index consists of a table which maps documents to ids and a number of index parts.
data InvIndex    = InvIndex { docTable :: !Documents
                             , indexParts :: !Parts 
                             } deriving (Show)

-- | The index parts are identified by a name, which should denote the context of the words.
type Parts       = Map Context Part
-- | The index part is the real inverted index. Words are mapped to their occurrences.
type Part        = StrMap Occurrences


instance HolIndex InvIndex where
  empty = InvIndex emptyDocuments M.empty

  sizeDocs = IM.size . idToDoc . docTable
  sizeWords = M.fold ((+) . SM.size) 0 . indexParts
  documents = docTable
  contexts = map fst . M.toList . indexParts

  allWords c i = SM.toList $ getPart c i
  prefixCase c i q = SM.prefixFindNoCaseWithKey q $ getPart c i
  prefixNoCase c i q = SM.prefixFindNoCaseWithKey q $ getPart c i
  lookupCase c i q = maybeToList (SM.lookup q $ getPart c i)
  lookupNoCase c i q = SM.lookupNoCase q $ getPart c i

  insert _ _ _ _ _ = empty -- TODO: This is just a dummy
  update _ _ _ _ _ = empty -- TODO: This is just a dummy
  
  loadFromFile f = runX (xunpickleDocument xpInvIndex 
                   			 [ (a_remove_whitespace, v_1)	
				   			 , (a_validate, v_0)
				   			 ] f)
				

-- | Return a part of the index for a given context.
-- TODO Shouldn't this be in Common.hs?
getPart :: Context -> InvIndex -> Part
getPart c i = fromMaybe SM.empty (M.lookup c $ indexParts i)
--------------------------------------------------------------------------------

-- pickler functions

xpInvIndex :: PU InvIndex
xpInvIndex = xpElem "indexes" $
	    xpickle

instance XmlPickler InvIndex where
    xpickle =  xpWrap ( \(dt, ip) -> InvIndex dt ip
                      , \(InvIndex dt ip) -> (dt, ip)
		                  )
   	(xpPair																									-- process doctable and indexparts
		(xpElem "documents" xpickle)											-- <DOCUMENTS>
		(xpWrap (M.fromList, M.toList)(xpList																-- Parts <-> [(Context, Part)]
			(xpElem "part" (xpPair												-- <PART			
				(xpAttr "id" xpText)											--       ID="">
				(xpElem "index"													--   <INDEX>        
					(xpWrap (SM.fromList, SM.toList) (xpList													-- Part <-> [(Word, Occurences)]
					(xpElem "word" (xpPair										--     <WORD
						(xpAttr "w" xpText)										--           W="">
						(xpWrap (IM.fromList, IM.toList) (xpList											-- Occurences <-> [(DocId, Positions)]
							(xpElem "doc" (xpPair								--       <DOC
								(xpAttr "idref" xpPrim)							--            IDREF="">
								(xpWrap	
									( IS.fromList . Prelude.map read . words  								-- Positions -> String 
									, unwords . Prelude.map show . IS.toList									-- String -> Positions
									) xpText
								)
							))
						))
					))
				)))
			))
		))
	)


