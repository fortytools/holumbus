{-# LANGUAGE OverloadedStrings #-}

-- ------------------------------------------------------------

module Hayoo.Hunt.FctRankTable
where

import           Control.Applicative         ((<$>))
-- import           Control.DeepSeq
-- import           Control.Monad

import           Data.Aeson
import qualified Data.ByteString.Lazy        as LB
import qualified Data.Map.Strict             as SM
import qualified Data.Text                   as T

-- import           Hayoo.FunctionInfo
-- import           Hayoo.Hunt.ApiDocument
import           Hayoo.Hunt.IndexSchema
import           Hayoo.Hunt.Output
-- import           Hayoo.IndexTypes

import           Hunt.Common.ApiDocument
import           Hunt.Interpreter.Command
import           Hunt.Query.Language.Grammar
import           Hunt.Query.Result           (Score)

-- ------------------------------------------------------------

type FctRankTable = SM.Map T.Text Float

lookupRank :: T.Text -> FctRankTable -> Maybe Float
lookupRank = SM.lookup

fromCommand :: Command -> FctRankTable
fromCommand (Sequence cs)
    = SM.fromList . concatMap cmdToRank $ cs
      where
        cmdToRank (Update d)
            = docToRank d
        cmdToRank (Sequence cs')
            = concatMap cmdToRank cs'
        cmdToRank _
            = []

        docToRank d
            | wght /= 1.0 = [(pkg, wght)]
            | otherwise   = []
            where
              pkg  = T.copy .           -- prevent sharing of Text values
                     last . T.split (== '/') . adUri $ d
              wght = maybe 1.0 id . adWght $ d

fromCommand _
    = SM.empty

rankFromFile :: FilePath -> IO FctRankTable
rankFromFile fn
    = (fromCommand . maybe NOOP id . decode) <$> LB.readFile fn

rankFromServer :: String -> IO FctRankTable
rankFromServer uri
    = do r1 <- (toJ . maybe "" id) <$> outputValue (Right uri) c
         print r1
         r2 <- toLimitedResult r1
         print r2
         r3 <- return $ toFT r2
         print r3
         return r3
    where
      c = Search q 0 (-1) True (Just [])
      q = QContext [c'type] $
          QPhrase QCase     $
          "package"

      toJ :: LB.ByteString -> Maybe (CmdRes (LimitedResult (ApiDocument, Score)))
      toJ = decode

      toLimitedResult :: Maybe (CmdRes (LimitedResult (ApiDocument, Score))) ->
                         IO            (LimitedResult (ApiDocument, Score))
      toLimitedResult Nothing
          = ioError . userError $
            "server error: syntax error in JSON response for rank table"
      toLimitedResult (Just (CmdRes r))
          = return r

      toFT :: LimitedResult (ApiDocument, Score) -> FctRankTable
      toFT r = SM.fromList . map toUW . map fst . lrResult $ r
          where
            toUW d = (adUri d, maybe 1.0 id $ adWght d)

-- ------------------------------------------------------------
