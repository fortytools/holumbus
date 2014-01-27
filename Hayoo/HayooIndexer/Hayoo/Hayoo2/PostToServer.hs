{-# LANGUAGE OverloadedStrings #-}

module Hayoo.Hayoo2.PostToServer
where

import qualified Data.ByteString.Lazy as LB
import           Data.Maybe

import           Network.Browser
import           Network.HTTP
import           Network.URI

-- ------------------------------------------------------------

type Req = Request  Bytes
type Res = Response Bytes

type Bytes = LB.ByteString

mkPostReq :: String -> String -> Bytes -> Req
mkPostReq url action bs
    = replaceHeader HdrContentType   "application/json" $
      replaceHeader HdrAccept        "application/json" $
      replaceHeader HdrUserAgent     "hayooCrawler/0.0.0.1" $
      setBody $
      mkReq
    where
      mkReq :: Req
      mkReq
          = mkRequest POST
            (fromJust $ parseURIReference $ url ++ "/" ++ action)

      setBody :: Req -> Req
      setBody rq
          = replaceHeader HdrContentLength (show l) $
            rq { rqBody = bs }
          where
            l = LB.length bs

defaultServer :: String
defaultServer = "http://localhost:3000/document"

postToServer :: Req -> IO ()
postToServer req
    = do res <- snd `fmap`
                (browse $ do setOutHandler (const $ return ()) -- disable trace output
                             request req
                )
         case rspCode res of
           (2,0,0) -> return ()
           _       -> ioError . userError . show $ res

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

