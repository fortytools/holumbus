module Main where

import System.IO

import Control.Exception
import Control.Monad

import Network
import Network.CGI

import Data.Binary
import Data.Int
import Data.Maybe

import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap as IM
import qualified Data.List as L
import qualified Data.Map as M

import Holumbus.Index.Common
import Holumbus.Query.Distribution.Protocol
import Holumbus.Query.Language.Grammar
import Holumbus.Query.Language.Parser
import Holumbus.Query.Result hiding (null)
import Holumbus.Build.Config

import System.Environment
import System.Posix


-- import Network.CGI
import Text.XML.HXT.Arrow

logoPath :: String
logoPath = "/logo.png"

cssPath :: String
cssPath = "/holumbus.css"

templatePath :: String
templatePath = "/home/sms/workspace/Holumbus/searchengine/examples/cgicomplete/template/template.xml"

makePage r a = 
      readDocument standardReadDocumentAttributes templatePath
  >>> processXPathTrees (sattr "href" cssPath )    "/html/head/link[@rel='stylesheet']/@href"
  >>> processXPathTrees (sattr "src"  logoPath)    "//div[@id='logo']/a/img[@class='logo']/@src"
  >>> processXPathTrees (a) "//div[@id='result']"
--  >>> writeDocument [("a_indent", "1")] "-"

getResult r = mkelem "div" [ sattr "id" "result"] 
                           [ mkelem "div" [sattr "id" "status"]   [ constA getStatus >>> mkText ]
                           , mkelem "div" [sattr "id" "document"] [ selem "ul" getDocuments ]
                           ]
  where 
  getStatus = "Found " ++ (show $ IM.size $ docHits r)  ++ " results and "
                       ++ (show $ M.size $ wordHits r) ++ " completions."
  getDocuments = IM.fold (\(i, _) l -> l ++ [mkelem "li" [] 
                                                         [mkelem "a" [sattr "href" (uri $ document i)]
                                                                     [ (constA $ uri $ document i) >>> mkText]
                                                         ]
                                            ] )
                         [] (docHits r)



main :: IO ()
main = runCGI $ handleErrors cgiMain


cgiMain :: CGI CGIResult
cgiMain =
  do
  setHeader "Content-type" "text/html"
  mn <- getInput "querytext"
  if (isJust mn)
    then do
         let pr = parseQuery $ fromJust mn
         theResult <- liftIO (either (\_ -> return emptyResult) (\q -> sendRequest theServer q) pr) 
         -- putStrLn "Content-type: text/html\n\n"
         res <- liftIO $ runX  (makePage theResult (getResult theResult) >>> writeDocumentToString [])
         output $ head res
    else do
         res <- liftIO $ runX (makePage emptyResult (mkelem "div" [ sattr "id" "result"]
                                                                  [ mkelem "div" [sattr "id" "status"] 
                                                                                 [constA "Enter some search terms above to start a search." >>> mkText]               
                                                                  ] ) >>> writeDocumentToString [])
         output $ head res
--  putStrLn (show args)
--  return ()  
  
getPrintable :: Result Int -> String
getPrintable r =  IM.fold processHit "" (docHits r)
  where 
  processHit :: (DocInfo Int, DocContextHits) -> String -> String
  processHit (i, _) s = s ++ (uri (document i)) 
  
theServer :: String
theServer = "localhost:4242"

-- | Send the query to a server and merge the result with the global result.
sendRequest :: Server -> Query -> IO (Result Int)
sendRequest s q = 
  withSocketsDo $ do 
    installHandler sigPIPE Ignore Nothing
    bracket (connectTo (getHost s) (getPort s)) (hClose) (send)
    where
    send hdl = hSetBuffering hdl NoBuffering >> sendQuery q hdl
    
sendQuery :: Query -> Handle -> IO (Result Int)
sendQuery q hdl =
  do
  enc <- return (encode q)
  -- Tell the server the type of the request and the length of the ByteString to expect.
  hPutStrLn hdl (queryCmd ++ " " ++ (show $ B.length enc))
  B.hPut hdl enc
  -- Get the length of the ByteString to expect.
  rsp <- getResponse hdl
  
  either (\s -> return (emptyResult)) processResponse rsp
  
    where
    processResponse len =
      do
      raw <- B.hGet hdl len
      -- Decode (and possibly decompress) the result
      res <- return (decode raw)
      -- Merge with the global result.
      return $! res
  
  
-- | Read the header from the handle and figure out if the request was successful. The left value
-- represents failure and contains the error message while the right value contains the length of
-- the returned data.
getResponse :: Handle -> IO (Either String Int)
getResponse hdl =
  do
  hdr <- liftM words $ hGetLine hdl
  if null hdr then return (Left "empty header") else
    if (head hdr) == successCode then 
      if null (tail hdr) then return (Right 0) 
      else return (Right $ read $ head $ tail hdr) 
    else 
      if null (tail hdr) then return (Left "no error message") 
      else return (Left $ L.intercalate " " (tail hdr))  
  
-- | Extract a host name from the server string.
getHost :: Server -> HostName
getHost s = takeWhile (/= ':') s

-- | Try to extract a port identifier from the server string. Fault tolerance needs to be improved.
getPort :: Server -> PortID
getPort _ = PortNumber 4242
-- getPort s = let r = dropWhile (/= ':') s in
--              if null r then PortNumber defaultPort else
--                if null $ tail r then PortNumber defaultPort else
--                  PortNumber (fromIntegral (read $ tail r :: Int))  
  
  
  
  
  
  