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
import           Hayoo.URIConfig

import           Hayoo.ParseSignature         (Signature, complexSignatures,
                                               processSignatureWith,
                                               subSignatures)
import           Hayoo.Url                    (urlForDocument)

import           Holumbus.Crawler
import           Holumbus.Crawler.IndexerCore

import           Hunt.ClientInterface         hiding (URI)

import           Text.XML.HXT.Core

-- {-
import           Debug.Trace                  (traceShow)

trc1 :: Show a => String -> a -> a
trc1 msg x = traceShow (msg, x) x

-- -}

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
insertHayooFctM (rawUri, rawDoc@(rawContexts, _rawTitle, rawCustom))
                ixs@(IndexerState _ (RDX dt))
    | nullContexts           -- no words found in document, so there are no refs in index
      ||
      nullFctInfo rawCustom  -- unknown Haddock element
        = return ixs         -- throw the suff away

    | otherwise
        = return $!
          IndexerState { ixs_index = ()
                       , ixs_documents = RDX $ M.insert rawUri rawDoc dt
                       }
    where
    nullContexts
        = and . map (null . snd) $ rawContexts

    nullFctInfo Nothing
        = True
    nullFctInfo (Just fi)
        | null (package fi)
          ||
          (fctType fi == Fct'unknown)
              = True
    nullFctInfo _
        = False

-- ------------------------------------------------------------

rewriteHrefs :: String -> String -> String -> String
rewriteHrefs pkg text uri
    = -- traceShow ("rewriteHrefs: pkg=", pkg, "text=", text, "uri=", uri, "res=", res) $
      res
    where
      uri'  = takeWhile (/= '#') uri                             -- throw the anchor away
      uri'' = hackagePackages ++ drop (length ("/package/"::String)) uri'  -- make an absolut hackage uri

      res
          | external    = uri                                  -- external ref: no edit
          | internal    = buildQuery  pkg   (modn uri') text   -- ref into haddock for this package
          | haddock     = buildQuery hpkg  (hmodn uri') text   -- ref into haddock for other package
          | otherwise   = ""                                   -- unknown ref:  throw away

      external
          = isExternalURI uri
            &&
            text == uri

      internal
          = isRelHaddockURI uri'

      haddock
          = "/package/" `L.isPrefixOf` uri'
            &&
            isHaddockURI uri''

      modn
          = map (\ x -> if x == '-' then '.' else x)
           . takeWhile (/= '.')

      hmodn
          = modn
            . concat . take 1 . reverse
            . words
            . map (\x -> if x == '/' then ' ' else x)

      hpkg
          = hackageGetPackage uri''

      buildQuery pkg' mod' name'
          = T.unpack $ urlForDocument (T.pack pkg') (T.pack mod') (T.pack name')

-- ------------------------------------------------------------

toCommand :: FctRankTable
          -> Bool
          -> UTCTime
          -> Bool
          -> [String]
          -> FctIndexerState
          -> Command
toCommand rt save now update pkgs (IndexerState _ (RDX ix))
    = appendSaveCmd save now $
      cmdSequence [ deletePkgCmd
                  , cmdSequence
                    . concatMap (toCmd rt dupMap now ix)
                    . M.toList
                  $ ix
                  ]
    where
      deletePkgCmd
          | update && not (null pkgs)
              = cmdDeleteDocsByQuery
                . setContext c'package
                . qOrs
                . map (qFullWord . T.pack)
                $ pkgs
          | otherwise
              = cmdNOOP

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
      insertCmd = (:[]) . cmdInsertDoc

      apiDoc    :: ApiDocument
      apiDoc    = toApiDoc $ ( T.pack k
                             , (cx, t, fmap FD cu)
                             , maybe 1.0 id . lookupRank pkg $ rt
                             )

      -- HACK: add the type attribute of the custom info record
      -- to a classifying context with name "type"
      addTypeToApiDoc   :: ApiDocument
      addTypeToApiDoc   = addToIndex c'type tp apiDoc
        where
          tp = T.pack $ maybe "" (drop 4 . show . fctType) cu

      addIndexedToApiDoc   :: ApiDocument
      addIndexedToApiDoc   = addDescription d'indexed now''
                             $ addToIndex c'indexed now'
                             $ changeIndex changeSignature
                             $ addTypeToApiDoc
        where
          now'  = fmtDateXmlSchema now
          now'' = fmtDateHTTP      now

      addModulesAndUrisToApiDoc u
          = addListOfModules
            $ addListOfUris
            $ addTypeToApiDoc
        where
          addListOfModules = addDescription d'module ms
          addListOfUris    = addDescription d'uris   us
          us = T.pack . show                 $ u
          ms = T.pack . show . map toModName $ u
              where
                toModName :: String -> String
                toModName u' = maybe "" id $
                               do (_, _, cu') <- M.lookup u' ix
                                  fd          <- cu'
                                  return (moduleName fd)

      changeSignature :: IndexMap -> IndexMap
      changeSignature cm0
          = -- trc1 "changeSignature" $
            (changeSig c'signature  (complexSignatures 0 . (:[])))

          -- signatures are already normalized,
          -- this must also be done in query processing
          -- so normalized contexts are not longer in use

          -- . (changeSig c'normalized  normSignature)
          . (changeSig c'subsig     (complexSignatures 1 . subSignatures))
            -- the 1 must match the complexity in hayooFrontend Hayoo.Common

          -- . (changeSig c'subnorm    (take maxSubsignatures . complexSignatures 2
          --                           . normSignatures . subSignatures))
          $ SM.delete c'signature cm0
        where
          -- maxSubsignatures = 20

          oldSig = SM.lookup c'signature cm0

          changeSig :: Context -> (Signature -> [Signature]) -> IndexMap -> IndexMap
          changeSig c f cm
              = case maybe "" id newSig of
                  "" -> cm
                  s  -> SM.insert c s cm
              where
                newSig = -- trc1 "newSig" $
                         (T.pack . processSignatureWith f . T.unpack) <$> oldSig

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
