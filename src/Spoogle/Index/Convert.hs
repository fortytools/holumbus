-- ----------------------------------------------------------------------------

{- |
   Module     : Spoogle.Index.Convert
   Copyright  : Copyright (C) 2007 Timo B. Hübel
   License    : MIT

   Maintainer : Timo B. Hübel
   Maintainer : t.h@gmx.info
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   Conversion between Spoogle indexes and several other formats.

-}

-- ----------------------------------------------------------------------------

module Spoogle.Index.Convert where

import Spoogle.Index.Inverted

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Spoogle.Data.Patricia as P

import qualified Hyphoon.DocIndex as H

-- | Converts an inverted index from the Hyphoon format to the Spoogle format.
hyphoonToInvSpoogle :: H.DocIndex -> InvIndex
hyphoonToInvSpoogle (H.DI idx dt) = InvSpoogle (toDocuments dt) (toParts idx)

toParts :: H.Index -> Parts
toParts idx = M.foldWithKey (toParts') M.empty idx
  where
    toParts' :: H.DocPart -> H.WordIndex -> Parts -> Parts
    toParts' dp wi p = M.insert dp (toPart wi) p

toPart :: H.WordIndex -> Part
toPart widx = M.foldWithKey (toPart') P.empty widx
  where
    toPart' :: H.Word -> H.Occurences -> Part -> Part
    toPart' w o p = P.insert w o p

toDocuments :: H.DocTable -> Documents
toDocuments (H.DT dm _ _ _) = IM.foldWithKey (toDocuments') emptyDocuments dm
  where
    toDocuments' :: Int -> (H.DocName, H.DocTitle) -> Documents -> Documents
    toDocuments' k (n, _) (DocTable i2d d2i) = DocTable (IM.insert k n i2d) (M.insert n k d2i)