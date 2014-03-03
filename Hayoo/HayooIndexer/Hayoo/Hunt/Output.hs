{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- ------------------------------------------------------------

module Hayoo.Hunt.Output
where
import           Control.Monad.IO.Class

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy     as LB
import           Data.Maybe
import           Data.Monoid

import           Network.Browser
import           Network.HTTP
import           Network.URI

import           System.Directory
import           System.FilePath

-- ------------------------------------------------------------

outputValue :: (MonadIO m, ToJSON c) => Either String String -> c -> m ()
outputValue (Left fn) = liftIO . jsonOutput True toFile
    where
      toFile bs
          = do createDirectoryIfMissing True dirPath
               LB.writeFile file bs
          where
            (dirPath, _) = splitFileName file
            file         = jsonPath fn

outputValue (Right uri) = liftIO . jsonOutput False toServer
    where
      toServer bs
          = postToServer $ mkPostReq (jsonUri uri) bs

jsonPath :: String -> String
jsonPath fn = "json" </> fn <.> "js"

jsonUri :: String -> String
jsonUri uri = uri </> "eval"

-- ------------------------------------------------------------

jsonOutput :: (ToJSON c) => Bool -> (LB.ByteString -> IO ()) -> c -> IO ()
jsonOutput pretty io x
    = io $ (if pretty then encodePretty' encConfig else encode) x
      where
        encConfig :: Config
        encConfig
            = Config { confIndent = 2
                     , confCompare
                         = keyOrder ["uri", "description", "index"]
                           `mappend`
                           compare
                     }

-- ------------------------------------------------------------

type Req = Request  Bytes
type Res = Response Bytes

type Bytes = LB.ByteString

mkPostReq :: String -> Bytes -> Req
mkPostReq uri bs
    = replaceHeader HdrContentType   "application/json" $
      replaceHeader HdrAccept        "application/json" $
      replaceHeader HdrUserAgent     "hayooCrawler/0.0.0.1" $
      setBody $
      mkReq
    where
      mkReq :: Req
      mkReq
          = mkRequest POST
            (fromJust $ parseURIReference $ uri)

      setBody :: Req -> Req
      setBody rq
          = replaceHeader HdrContentLength (show l) $
            rq { rqBody = bs }
          where
            l = LB.length bs

postToServer :: Req -> IO ()
postToServer req
    = do res <- snd `fmap`
                (browse $ do setOutHandler (const $ return ()) -- disable trace output
                             request req
                )
         case rspCode res of
           (2,0,0) -> return ()
           _       -> ioError . userError . show $ res

defaultServer :: String
defaultServer = "http://localhost:3000/"

-- ------------------------------------------------------------

{-
main :: IO ()
main
    = do bs <- L.readFile "../test/ttt.js"
         -- print req
         -- (_, res) <- browse $ request req
         -- print res
         res <- postToServer $ mkPostReq defaultServer "insert" bs
         print res
-- -}

-- ------------------------------------------------------------

