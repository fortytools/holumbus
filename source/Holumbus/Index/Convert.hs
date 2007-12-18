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

import Holumbus.Index.Common

import qualified Holumbus.Index.Inverted as INV
--import qualified Holumbus.Index.Hybrid as HYB
import qualified Holumbus.Index.DocIndex as H

import qualified Holumbus.Data.StrMap as SM

import qualified Data.Map as M
import qualified Data.IntMap as IM
--import qualified Data.IntSet as IS


-- | Converts an inverted index from the Hyphoon format to the Holumbus inverted file format.
hyphoonToInvHolumbus :: H.DocIndex -> INV.InvIndex
hyphoonToInvHolumbus (H.DI idx dt) = INV.InvIndex (toDocuments dt) (toPartsInv idx)

toPartsInv :: H.Index -> INV.Parts
toPartsInv idx = M.foldWithKey (toPartsInv') M.empty idx
  where
    toPartsInv' :: H.DocPart -> H.WordIndex -> INV.Parts -> INV.Parts
    toPartsInv' dp wi p = M.insert dp (toPartInv wi) p

toPartInv :: H.WordIndex -> INV.Part
toPartInv wi = M.foldWithKey (toPartInv') SM.empty wi
  where
    toPartInv' :: H.Word -> H.Occurences -> INV.Part -> INV.Part
    toPartInv' w o p = SM.insert w o p

{- This could server as starting point for conversion to Holumbus hybrid index.

-- | Converts an inverted index from the Hyphoon format to the Holumbus hybrid format.
hyphoonToHybHolumbus :: H.DocIndex -> HYB.HybIndex
hyphoonToHybHolumbus (H.DI idx dt) = HYB.HybHolumbus (toDocuments dt) (toPartsHyb idx)

toPartsHyb :: H.Index -> HYB.Parts
toPartsHyb idx = M.foldWithKey (toPartsHyb') M.empty idx
  where
    toPartsHyb' :: H.DocPart -> H.WordIndex -> HYB.Parts -> HYB.Parts
    toPartsHyb' dp wi p = M.insert dp (toPartHyb wi) p

toPartHyb :: H.WordIndex -> HYB.Part
toPartHyb wi = M.foldWithKey (toPartHyb') HYB.emptyPart wi
  where
    toPartHyb' :: H.Word -> H.Occurences -> HYB.Part -> HYB.Part
    toPartHyb' w o p = IM.foldWithKey (\d ps r -> IS.fold (HYB.insertOccurrence w d) r ps) p o
-}

toDocuments :: H.DocTable -> Documents
toDocuments (H.DT dm _ _ _) = IM.foldWithKey (toDocuments') emptyDocuments dm
  where
    toDocuments' :: Int -> (H.DocName, H.DocTitle) -> Documents -> Documents
    toDocuments' k (n, t) (DocTable i2d d2i _) = DocTable (IM.insert k (t, n) i2d) (M.insert n k d2i) k