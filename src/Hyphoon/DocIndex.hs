-- ------------------------------------------------------------

{- |
   Module     : Hyphoon.DocIndex
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt
   Maintainer : uwe@fh-wedel.de
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   Haskell XML/HTML Document Index Datatype

-}

-- ------------------------------------------------------------

module Hyphoon.DocIndex
where

import Text.XML.HXT.Arrow		-- import all stuff for parsing, validating, and transforming XML

import           Data.Char
import           Data.List

import           Data.IntMap	( IntMap )
import qualified Data.IntMap as IM

import           Data.Map	( Map )
import qualified Data.Map as M

import           Data.IntSet	( IntSet )
import qualified Data.IntSet as IS

-- ------------------------------------------------------------

-- | The DocIndex consists of 2 parts, "theIndex" is the real invertet index
-- but for space efficiency the document names are mapped to document ids.
-- This mapping is stored in "theDocTable"

data DocIndex	= DI { theIndex    :: ! Index
		     , theDocTable :: ! DocTable
		     }
		  deriving (Show)

-- | An index is separated in parts. In a part there is the
-- inverted index for all words in a document part, e.g. for the contents
-- of a document the words are stored in a content-part, for the headlines
-- in a headline-part and for the keywords in a keyword-part. A part is identified
-- by a name (a string).

type Index	= Map DocPart WordIndex


-- | A word index is a mapping of words to occurences.

type WordIndex	= Map Word Occurences


-- | The occurences are again a map from DocIds to a set op positions

type Occurences	= IntMap Positions	-- Map DocId Positions


-- | The positions are a set of ints denoting word positions in a document

type Positions	= IntSet		-- Set Position

type Position	= Int
type DocPart	= String
type Word	= String


-- | The table containing all document names and ids of an index.
--
-- Because this is a bijective map, the inverse map is stored in the
-- 2. component, the cardinality forms the 3. component. The last component
-- is a list of all unused docIds for generating new ids for new documents

data DocTable	= DT { docId2docName	:: ! DocMap
		     , docName2docId    :: ! DocMap_1
		     , noOfDocs         :: ! Int
		     , unusedDocIds     :: [Int]
		     }
		  deriving (Show)

type DocId	= Int
type DocName	= String
type DocTitle	= String
type DocMap	= IntMap (DocName, DocTitle)	-- Map DocId (DocName, DocTitle)
type DocMap_1   = Map DocName DocId		-- the inverse map

-- ------------------------------------------------------------

-- DocTable ops

emptyDocTable	:: DocTable
emptyDocTable	= DT { docId2docName = IM.empty
		     , docName2docId = M.empty
		     , noOfDocs      = 0
		     , unusedDocIds  = [1..]
		     }

-- | insert a new document into the document table

insertDoc	:: DocId -> DocName -> DocTitle -> DocTable -> DocTable
insertDoc did dn dti (DT dm im cnt uids)
    | did `IM.member` dm
	= error ("insertDoc: docId "   ++ show did ++ " already in table")
    | dn   `M.member` im
	= error ("insertDoc: docName " ++ show dn  ++ " already in table")
    | otherwise
	= DT (IM.insert did (dn, dti) dm) (M.insert dn did im) (cnt + 1) (remId did uids)
    where
    remId i (x:xs)
	| i > x  = x : remId i xs
	| i == x = xs
    remId _i _xs
	= error ("insertDoc: docId " ++ show did ++ " already in use")

insertDocs	:: [(DocId, (DocName, DocTitle))] -> DocTable -> DocTable
insertDocs dl dt
    = foldl ins dt dl
    where
    ins dt' (did', (dn', dti')) = insertDoc did' dn' dti' dt'

-- | remove a docId

removeDocId	:: DocId -> DocTable -> DocTable
removeDocId did (DT dm im cnt uids)
    | not (did `IM.member` dm)
	= error ("removeDocId: docId " ++ show did ++ " not in table")
    | otherwise
	= DT (IM.delete did dm) (M.delete (fst (dm IM.! did)) im) (cnt - 1) (did : uids)

newDocIds	:: Int -> DocTable -> [DocId]
newDocIds i dt
    = take i . unusedDocIds $ dt

-- ------------------------------------------------------------
--

emptyDocIndex :: DocIndex
emptyDocIndex = DI M.empty emptyDocTable

-- | remove document with given id

removeDoc	:: DocId -> DocIndex -> DocIndex
removeDoc did dx@(DI ix dt)
    | did `IM.member` (docId2docName dt)
	= DI ix' dt'
    | otherwise
	= dx
    where
    ix' = M.map removeInWordIndex ix			-- remove did in all parts
    dt' = removeDocId did dt

    removeInWordIndex					-- remove all occurences in all word idx
	= M.filter (not . IM.null)			-- and remove unused words
	  . M.map removeInOccurences

    removeInOccurences					-- just delete in did in occurence map
	= IM.delete did

-- ------------------------------------------------------------
--
-- merge 2 document indexes
--
-- the operation is not symmetric, when both indexes contain
-- the same documents, the 1 index overwrites the entries in the 2. one,
-- so the 1. index is considered to be the new index

mergeDocIndexes	::  DocIndex -> DocIndex -> DocIndex
mergeDocIndexes nx ox
    = mergeDisjointIndexes nx (removeOldDocs nx ox)

-- | remove all docs in 2. (old) index, which occur in 1. (new) index

removeOldDocs	:: DocIndex -> DocIndex -> DocIndex
removeOldDocs (DI _nix ndt) ox@(DI _oix odt)
    = IM.fold remDoc ox (docId2docName ndt)
    where
    remDoc (n, _t) ix
	= maybe ix ((flip removeDoc) ix) . M.lookup n . docName2docId $ odt

-- | merge 2 disjoint indexes

mergeDisjointIndexes	::  DocIndex -> DocIndex -> DocIndex
mergeDisjointIndexes (DI nix ndt) (DI oix odt)
    = DI rix rdt
    where
    rix = mergeIndexes (renameDocIds rename nix) oix
    rdt = insertDocs docs odt
    docs = map (\ (x,y) -> (rename x, y)) . IM.toList . docId2docName $ ndt
    rename oid
	= oidnidMap IM.! oid
	where
	nids = newDocIds (noOfDocs ndt) odt
	oids = IM.keys . docId2docName $ ndt
	oidnidMap = IM.fromList (zip oids nids)

-- ------------------------------------------------------------
--
-- operations on word indexes

-- | rename all docIds to not overlap with another index

renameDocIds	:: (DocId -> DocId) -> Index -> Index
renameDocIds nids ix
    = M.map renameInWordIndex $ ix
    where
    renameInWordIndex
	= M.map renameInOccurences
    renameInOccurences
	= IM.fromList . map (\ (i,n) -> (nids i,n)) . IM.toList

-- | merge 2 disjoint indexes, that means the set of document ids is disjoint
--
-- the merge loop is done over the first parameter, so if this one is smaller
-- the operation is more efficient than the other way round
-- e.g. if the 1. index is generated from a single document it's rather efficient

mergeIndexes :: Index -> Index -> Index
mergeIndexes nix oix
    = M.unionWith mergeWordIndexes oix nix	-- arguments are flipped, see ghc Map docu for union
    where
    mergeWordIndexes
	= M.unionWith mergeOccurences
	where
	mergeOccurences
	    = IM.unionWith IS.union

-- ------------------------------------------------------------
--
-- | conversion from XML into internal representation

docIndexFromXml	:: ArrowXml a => a XmlTree DocIndex
docIndexFromXml
    = fromLA xml2doc
    where
    xml2doc	:: LA XmlTree DocIndex
    xml2doc
	= ( ( listA ( this
		      /> hasName "indexes"
		      /> hasName "part"
		      >>> ( getAttrValue0 "id"
			    &&&
			    getWordIx
			  )
		    )
	      >>^ M.fromList
	    )
	    &&&
	    ( listA ( this
		      /> hasName "indexes"
		      /> hasName "documents"
		      /> hasName "doc"
		      >>> ( ( getAttrValue0 "id" >>^ read )
			    &&&
			    getAttrValue0 "href"
			    &&&
			    getAttrValue "title"
			  )
		    )
	    )
	  )
          >>^ uncurry mkDI
	where
	mkDI ix dm
	    =  DI ix (insertDocs dm emptyDocTable)

    getWordIx	:: LA XmlTree WordIndex
    getWordIx
	= listA ( this
		  /> hasName "index"
		  /> hasName "word"
		  >>> ( getAttrValue0 "w"
			&&&
			getDocIx
		      )
		)
	  >>^ M.fromList

    getDocIx	:: LA XmlTree Occurences
    getDocIx
	= listA ( this
		  /> hasName "doc"
		  >>> ( ( getAttrValue0 "idref" >>^ read )
			&&&
			( xshow getChildren >>^ ( IS.fromList . map read . words) )
		      )
		)
	  >>^ IM.fromList

-- ------------------------------------------------------------
--
-- | conversion from internal representation into XML.
-- The inverse to "docIndexFromXml"
--
-- @docIndexFromXml >>> docIndexToXml === this@ and
-- @docIndexToXml >>> docIndexFromXml === this@


docIndexToXml :: ArrowXml a => a DocIndex XmlTree
docIndexToXml
    = fromLA doc2xml
    where
    doc2xml	:: LA DocIndex XmlTree
    doc2xml
	= root []
	  [ mkelem "indexes" []
	    [ ( mkelem "documents" []
		[ arrL (IM.toList . docId2docName . theDocTable)
		  >>>
		  ( mkelem "doc"
		    [ attr "id"   ((show . fst) ^>> mkText)
		    , attr "href"  ((fst . snd) ^>> mkText)
		    , attr "title" ((snd . snd) ^>> mkText)
		    ] []
		  )
		]
	      )
	      <+>
	      ( arrL (M.toList . theIndex)
		>>>
		( mkelem "part"
		  [ attr "id" (fst ^>> mkText) ]
		  [ mkelem "index" []
		    [ arrL (M.toList . snd) >>> mkWordIx ]
		  ]
		)
	      )
	    ]
	  ]

    mkWordIx	:: LA (Word, Occurences) XmlTree
    mkWordIx
	= mkelem "word" [attr "w" (fst ^>> mkText)]
	  [ arrL (IM.toList . snd)
	    >>>
	    mkelem "doc"
	    [ attr "idref" ((show . fst) ^>> mkText) ]
	    [ (unwords . map show . IS.toList . snd) ^>> mkText ]
	  ]

-- ------------------------------------------------------------
