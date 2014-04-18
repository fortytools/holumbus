{-# LANGUAGE OverloadedStrings #-}

-- ------------------------------------------------------------

module Hayoo.Hunt.FctRankTable
where

import           Control.Applicative      ((<$>))
-- import           Control.DeepSeq
-- import           Control.Monad

import           Data.Aeson
import qualified Data.ByteString.Lazy     as LB
import qualified Data.Map.Strict          as SM
import qualified Data.Text                as T

-- import           Hayoo.FunctionInfo
-- import           Hayoo.Hunt.ApiDocument
-- import           Hayoo.Hunt.IndexSchema
-- import           Hayoo.IndexTypes

import           Hunt.Common.ApiDocument
import           Hunt.Interpreter.Command
-- import           Hunt.Query.Language.Grammar

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
rankFromServer _uri
    = return $ fromCommand NOOP
