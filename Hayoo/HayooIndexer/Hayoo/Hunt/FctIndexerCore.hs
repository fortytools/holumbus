{-# LANGUAGE OverloadedStrings #-}

-- ------------------------------------------------------------

module Hayoo.Hunt.FctIndexerCore
where

import           Control.Applicative          ((<$>))
import           Control.DeepSeq
import           Control.Monad

import           Data.Binary                  (Binary)
import qualified Data.Binary                  as B
import qualified Data.IntMap.Strict           as IM
import qualified Data.List                    as L
import qualified Data.Map.Strict              as SM
import qualified Data.StringMap.Strict        as M
import qualified Data.Text                    as T
import           Data.Time                    (UTCTime)

import           Hayoo.FunctionInfo
import           Hayoo.Hunt.ApiDocument
import           Hayoo.Hunt.FctRankTable
import           Hayoo.Hunt.IndexSchema
import           Hayoo.IndexTypes

import           Hayoo.ParseSignature         (expand, expandNormalized,
                                               modifySignatureWith)

import           Holumbus.Crawler
import           Holumbus.Crawler.IndexerCore

import           Hunt.Interpreter.Command
import           Hunt.Query.Language.Grammar

import           Text.XML.HXT.Core

-- ------------------------------------------------------------

type FctCrawlerConfig   = IndexCrawlerConfig () RawDocIndex FunctionInfo
type FctCrawlerState    = IndexCrawlerState  () RawDocIndex FunctionInfo

type FctIndexerState    = IndexerState       () RawDocIndex FunctionInfo

newtype RawDocIndex a   = RDX (M.StringMap (RawDoc FunctionInfo))
                          deriving (Show)

instance NFData (RawDocIndex a)

instance Binary (RawDocIndex a) where
    put (RDX ix)        = B.put ix
    get                 = RDX <$> B.get

emptyFctState           :: FctIndexerState
emptyFctState           = emptyIndexerState () emptyRawDocIndex

emptyRawDocIndex        :: RawDocIndex a
emptyRawDocIndex        = RDX $ M.empty

insertRawDoc            :: URI -> RawDoc FunctionInfo -> RawDocIndex a -> RawDocIndex a
insertRawDoc uri rd (RDX ix)
                        = rnf rd `seq` (RDX $ M.insert uri rd ix)

-- ------------------------------------------------------------

unionHayooFctStatesM        :: FctIndexerState -> FctIndexerState -> IO FctIndexerState
unionHayooFctStatesM (IndexerState _ (RDX dt1)) (IndexerState _ (RDX dt2))
    = return $!
      IndexerState { ixs_index     = ()
                   , ixs_documents = RDX $ M.union dt1 dt2
                   }

insertHayooFctM :: (URI, RawDoc FunctionInfo) ->
                   FctIndexerState ->
                   IO FctIndexerState
insertHayooFctM (rawUri, rawDoc@(rawContexts, _rawTitle, _rawCustom))
                ixs@(IndexerState _ (RDX dt))
    | nullContexts
        = return ixs    -- no words found in document,
                        -- so there are no refs in index
                        -- and document is thrown away
    | otherwise
        = return $!
          IndexerState { ixs_index = ()
                       , ixs_documents = RDX $ M.insert rawUri rawDoc dt
                       }
    where
    nullContexts
        = and . map (null . snd) $ rawContexts

toCommand :: FctRankTable -> Bool -> UTCTime -> Bool -> [String] -> FctIndexerState -> Command
toCommand rt save now update pkgs (IndexerState _ (RDX ix))
    = appendSaveCmd save now $
      Sequence [ deletePkgCmd
               , Sequence . concatMap (toCmd rt dupMap now ix). M.toList $ ix
               ]
    where
      deletePkgCmd
          | update && not (null pkgs)
              = DeleteByQuery                $
                QContext [c'package]         $
                foldr1 (QBinary Or)          $
                map (QPhrase QCase . T.pack) $
                pkgs
          | otherwise
              = NOOP

      -- compute duplicates generated by re-exports of functions
      dupMap
          = toDup ix

toCmd    :: FctRankTable
            -> IM.IntMap [URI]
            -> UTCTime
            -> M.StringMap (RawDoc FunctionInfo)
            -> (M.Key, RawDoc FunctionInfo)
            -> [Command]
toCmd rt dupMap now ix (k, (cx, t, cu))
    = case lookupDup t cu dupMap of
        Just uris@(uri : _uris1)                  -- re-exports found
            | uri == k
                -> insertCmd (addModulesAndUrisToApiDoc uris) -- TODO: modify module attr and add all uris
            | otherwise
                -> []
        _       -> insertCmd addIndexedToApiDoc
    where
      pkg       = maybe "" id . fmap fiToPkg $ cu
      insertCmd = (:[]) . Insert
      apiDoc    = toApiDoc $ (T.pack k, (cx, t, fmap FD cu), lookupRank pkg rt)

      -- HACK: add the type attribute of the custom info record
      -- to a classifying context with name "type"
      addTypeToApiDoc   = insIndexMap c'type tp apiDoc
        where
          tp = T.pack $ maybe "" (drop 4 . show . fctType) cu

      addIndexedToApiDoc   = insDescrMap d'indexed now'' $
                             insIndexMap c'indexed now'  $
                             addTypeToApiDoc
        where
          now'  = fmtDateXmlSchema now
          now'' = fmtDateHTTP      now


      addModulesAndUrisToApiDoc u = addListOfModules $ addListOfUris $ changeSignature $ addTypeToApiDoc
        where
          addListOfModules = insDescrMap d'module ms
          addListOfUris    = insDescrMap d'uris   us
          us = T.pack . show                 $ u
          ms = T.pack . show . map toModName $ u
              where
                toModName :: String -> String
                toModName u' = maybe "" id $
                               do (_, _, cu') <- M.lookup u' ix
                                  fd          <- cu'
                                  return (moduleName fd)

      changeSignature = chgIndexMap (changeSig c'normalized expandNormalized) . chgIndexMap (changeSig c'signature expand)
        where
        -- changeSig :: Context -> (Signature -> [Signature]) -> SM.Map Context Content -> SM.Map Context Content
        changeSig c f cm = SM.insert c (maybe "" id newSig) cm
          where
          oldSig =  c `SM.lookup` cm
          newSig = (T.pack . modifySignatureWith f . T.unpack) <$> oldSig

lookupDup :: String -> Maybe FunctionInfo -> IM.IntMap [URI] -> Maybe [URI]
lookupDup n v m
    | not (isFct v) = Nothing
    | otherwise     = do h  <- fmap (fiToHash n) v
                         us <- IM.lookup h m
                         if null us || null (tail us)
                            then mzero
                            else return (L.sort us)

toDup :: M.StringMap (RawDoc FunctionInfo) -> IM.IntMap [URI]
toDup ix
    = IM.fromListWith (++) . concatMap to . M.toList $ ix
    where
      to (k, (_cx, t, v))
          | not (isFct v) = []
          | otherwise     = [(maybe 0 id . fmap (fiToHash t) $ v, [k])]

isFct :: Maybe FunctionInfo -> Bool
isFct Nothing   = False
isFct (Just fd) = fctType fd == Fct'function

{-
-- | The hash function from URIs to DocIds
docToId :: URI -> DocId
docToId = DId.fromInteger . fromIntegral . asWord64 . hash64 . B.encode
-- -}
-- ------------------------------------------------------------

-- the pkgIndex crawler configuration

indexCrawlerConfig :: SysConfig                                       -- ^ document read options
                      -> (URI -> Bool)                                -- ^ the filter for deciding, whether the URI shall be processed
                      -> Maybe (IOSArrow XmlTree String)              -- ^ the document href collection filter, default is 'Holumbus.Crawler.Html.getHtmlReferences'
                      -> Maybe (IOSArrow XmlTree XmlTree)             -- ^ the pre document filter, default is the this arrow
                      -> Maybe (IOSArrow XmlTree String)              -- ^ the filter for computing the document title, default is empty string
                      -> Maybe (IOSArrow XmlTree FunctionInfo)        -- ^ the filter for the cutomized doc info, default Nothing
                      -> [IndexContextConfig]                         -- ^ the configuration of the various index parts
                      -> FctCrawlerConfig                             -- ^ result is a crawler config

indexCrawlerConfig
    = indexCrawlerConfig' insertHayooFctM unionHayooFctStatesM

-- ------------------------------------------------------------
