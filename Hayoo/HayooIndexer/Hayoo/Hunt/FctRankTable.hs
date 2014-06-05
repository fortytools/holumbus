{-# LANGUAGE OverloadedStrings #-}

-- ------------------------------------------------------------

module Hayoo.Hunt.FctRankTable
where

import           Control.Applicative      ((<$>))

import           Data.Aeson
import qualified Data.ByteString.Lazy     as LB
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as SM
import           Data.Text                (Text)
import qualified Data.Text                as T

import           Hayoo.Hunt.IndexSchema
import           Hayoo.Hunt.Output

import           Hunt.ClientInterface     hiding (URI)
import           Hunt.Interpreter.Command (Command (..))

-- ------------------------------------------------------------

type FctRankTable = Map Text Float

lookupRank :: Text -> FctRankTable -> Maybe Float
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

fromCommand _
    = SM.empty

docToRank :: ApiDocument -> [(Text, Float)]
docToRank d
    = maybe [] (\ w -> [(pkg, w)]) $ wght
    where
      pkg  = T.copy .           -- prevent sharing of Text values
             last . T.split (== '/') . adUri $ d
      wght = getScore . adWght $ d

rankFromFile :: FilePath -> IO FctRankTable
rankFromFile fn
    = (fromCommand . maybe cmdNOOP id . decode) <$> LB.readFile fn

rankFromServer :: String -> IO FctRankTable
rankFromServer uri
    = do r1 <- (toJ . maybe "" id) <$> outputValue (Right uri) c
         -- print r1
         r2 <- toLimitedResult r1
         -- print r2
         r3 <- return $ toFT r2
         -- print r3
         return r3
    where
      c = setSelectedFields []         -- no fields, just the weight is needed
          . setWeightIncluded
          . cmdSearch
          . setContext c'type
          $ qPhrase "package"

      toJ :: LB.ByteString -> Maybe (CmdRes (LimitedResult ApiDocument))
      toJ = decode

      toLimitedResult :: Maybe (CmdRes (LimitedResult ApiDocument)) ->
                         IO            (LimitedResult ApiDocument)
      toLimitedResult Nothing
          = ioError . userError $
            "server error: syntax error in JSON response for rank table"
      toLimitedResult (Just (CmdRes r))
          = return r

      toFT :: LimitedResult ApiDocument -> FctRankTable
      toFT r = SM.fromList . concatMap docToRank . lrResult $ r

-- ------------------------------------------------------------
