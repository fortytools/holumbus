-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Convert
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Conversion between Holumbus indexes and several other formats.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Convert 
  (
  hyphoonToInvHolumbus
  )
where

import Holumbus.Index.Inverted

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Holumbus.Data.StrMap as SM

import qualified Holumbus.Index.DocIndex as H

-- | Converts an inverted index from the Hyphoon format to the Holumbus format.
hyphoonToInvHolumbus :: H.DocIndex -> InvIndex
hyphoonToInvHolumbus (H.DI idx dt) = InvHolumbus (toDocuments dt) (toParts idx)

toParts :: H.Index -> Parts
toParts idx = M.foldWithKey (toParts') M.empty idx
  where
    toParts' :: H.DocPart -> H.WordIndex -> Parts -> Parts
    toParts' dp wi p = M.insert dp (toPart wi) p

toPart :: H.WordIndex -> Part
toPart widx = M.foldWithKey (toPart') SM.empty widx
  where
    toPart' :: H.Word -> H.Occurences -> Part -> Part
    toPart' w o p = SM.insert w o p

toDocuments :: H.DocTable -> Documents
toDocuments (H.DT dm _ _ _) = IM.foldWithKey (toDocuments') emptyDocuments dm
  where
    toDocuments' :: Int -> (H.DocName, H.DocTitle) -> Documents -> Documents
    toDocuments' k (n, t) (DocTable i2d d2i) = DocTable (IM.insert k (t, n) i2d) (M.insert n k d2i)