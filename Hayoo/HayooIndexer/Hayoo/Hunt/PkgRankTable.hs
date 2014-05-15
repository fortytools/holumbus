{-# LANGUAGE OverloadedStrings #-}

-- ------------------------------------------------------------

module Hayoo.Hunt.PkgRankTable
where

import           Control.Applicative         ((<$>))

import           Data.Aeson
import qualified Data.ByteString.Lazy        as LB
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as SM
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time

import           Hayoo.Hunt.IndexSchema
import           Hayoo.Hunt.Output
import           Hayoo.PackageRank

import           Hunt.Common.ApiDocument
import           Hunt.Common.DocDesc         (unDesc)
import           Hunt.Interpreter.Command
import           Hunt.Query.Language.Grammar

-- ------------------------------------------------------------

type PkgMap    = Map Text Text
type RankTable = Ranking Text
type DepList   = DAGList Text

dependenciesFromServer :: FilePath -> IO (PkgMap, RankTable)
dependenciesFromServer uri
    = do r1 <- (toJ . maybe "" id) <$> outputValue (Right uri) c
         -- print r1
         r2 <- toLimitedResult r1
         -- print r2
         r3 <- return $ toDM r2
         -- print r3
         return r3
    where
      c = Search q 0 (-1) False (Just [d'name, d'dependencies])
      q = QContext [c'type] $
          QPhrase QCase     $
          "package"

      toJ :: LB.ByteString -> Maybe (CmdRes (LimitedResult ApiDocument))
      toJ = decode

      toLimitedResult :: Maybe (CmdRes (LimitedResult ApiDocument)) ->
                         IO            (LimitedResult ApiDocument)
      toLimitedResult Nothing
          = ioError . userError $
            "server error: syntax error in JSON response for package dependencies"
      toLimitedResult (Just (CmdRes r))
          = return r

      toDM :: LimitedResult ApiDocument -> (PkgMap, RankTable)
      toDM r = fromDepList . concatMap docToDep . lrResult $ r

dependenciesFromFile :: FilePath -> IO (PkgMap, RankTable)
dependenciesFromFile fn
    = toRes <$> LB.readFile fn
    where
      toRes = fromDepList . toDepList . maybe NOOP id . decode

fromDepList :: [(Text, (Text, [Text]))] -> (PkgMap, RankTable)
fromDepList xs
    = (SM.map fst pm, rt)
      where
        pm = SM.fromList xs
        rt = rankingStd . SM.elems $ pm

toDepList :: Command -> [(Text, (Text, [Text]))]
toDepList (Sequence cs)
    = concatMap cmdToDep cs
      where
        cmdToDep (Insert d)
            = docToDep d
        cmdToDep (Sequence cs')
            = concatMap cmdToDep cs'
        cmdToDep _
            = []
toDepList _
    = []

docToDep :: ApiDocument -> [(Text, (Text, [Text]))]
docToDep d
    | T.null name = []
    | otherwise   = [(adUri d, (name, deps))]
      where
        desc = unDesc . adDescr $ d
        name =           maybe "" id . SM.lookup d'name         $ desc
        deps = T.words . maybe "" id . SM.lookup d'dependencies $ desc

rankToCommand :: Bool -> UTCTime -> (PkgMap, RankTable) -> Command
rankToCommand save now (pm, rt)
    = appendSaveCmd save now .
      Sequence .
      map Update .
      concatMap toApiDoc . SM.toList $ pm
    where
      toApiDoc (uri, name)
          | wght == 1.0
              = []
          | otherwise
              = (:[]) $
                emptyApiDoc { adUri  = uri
                            , adWght = Just wght
                            }
          where
            wght = maybe 1.0 id . SM.lookup name $ rt

toCommand :: Bool -> UTCTime -> (Either String String) -> IO Command
toCommand save now source
    = rankToCommand save now <$> getRankInfo source
      where
        getRankInfo (Left fn)   = dependenciesFromFile $ jsonPath fn
        getRankInfo (Right uri) = dependenciesFromServer uri

-- ------------------------------------------------------------
