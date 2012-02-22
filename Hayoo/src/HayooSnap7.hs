{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Applicative
import           Control.Exception              ( SomeException )
import           Control.Monad
import           Control.Monad.CatchIO          ( catch )
import           Control.Monad.State

import qualified Data.ByteString.Char8          as B

import           Data.Lens.Common
import           Data.Lens.Template
import           Data.Map                       ( toList )
import           Data.Maybe

import           Data.Text (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T

import           Hayoo.IndexTypes               ( buildRankTable
                                                )
import           Hayoo.Search.EvalSearch        ( Core(..)
                                                , emptyRes
                                                , examples
                                                , filterStatusResult
                                                , genResult
                                                , getValDef
                                                , loadDocuments
                                                , loadIndex
                                                , loadPkgDocs
                                                , parseQuery
                                                , readDef
                                                , renderEmptyJson
                                                , renderJson
                                                , template
                                                )
import           Hayoo.Search.HTML              ( RenderState(..)
                                                , result
                                                )
import           Hayoo.Search.Pages.Template    ( makeTemplate
                                                )
import qualified Hayoo.Search.Pages.Static      as P

import           Holumbus.Index.Common          ( sizeDocs )
{-
import           Data.Time.Clock.POSIX
import           Foreign.C.Types
-- -}
import           Prelude                        hiding (catch)

import           Snap.Http.Server
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
{-
import           Snap.Util.GZip
-- -}
import           System.FilePath                ( (</>) )
import           System.IO                      ( stderr
                                                , hPutStrLn
                                                )
import           Text.Templating.Heist

import qualified Text.XHtmlCombinators          as X

------------------------------------------------------------------------------

type HayooState = Core

data App
    = App
      { _heist      :: Snaplet (Heist App)
      , _hayooState :: HayooState
      }

makeLenses [''App]

instance HasHeist App where
    heistLens = subSnaplet heist

------------------------------------------------------------------------------

{- snap website example stuff

epochTime :: IO CTime
epochTime = do
    t <- getPOSIXTime
    return $ fromInteger $ truncate t
-- -}

description :: Text
description = "The snapframework.com website"


appInit :: SnapletInit App App
appInit = makeSnaplet "snap-website" description Nothing $ do
    hs <- nestSnaplet "" heist $ heistInit "templates"
    hy <- liftIO $ getHayooInitialState

    addRoutes [ ("/",             ifTop hayooHtml)       -- map to /hayoo.html
              , ("/hayoo.html",   hayooHtml)
              , ("/hayoo.json",   hayooJson)
              , ("/help.html",    serveStatic P.help)
              , ("/about.html",   serveStatic P.about)
              , ("/api.html",     serveStatic P.api)
              , ("/hayoo/:stuff", serveHayooStatic)
              ]
    addSplices [ ("snap-version", serverVersion)
               , ("feed-autodiscovery-link", liftHeist $ textSplice "")
               ]
    wrapHandlers catch500

    {- snap website example stuff: compress html and set headers for caching static pages

    wrapHandlers (\h -> catch500 $ withCompression $
                        h <|> setCache (serveDirectory "static"))
    -- -}
    return $ App hs hy

------------------------------------------------------------------------------
-- | Deliver Hayoo files

serveHayooStatic :: Handler App App ()
serveHayooStatic = do
    relPath <- decodedParam "stuff"
    serveFile $ "hayoo" </> B.unpack relPath
  where
    decodedParam p = fromMaybe "" <$> getParam p

------------------------------------------------------------------------------
-- | Deliver static Hayoo pages

serveStatic :: X.XHtml X.FlowContent -> Handler App App ()
serveStatic pg
  = do
    core <- hayooCore
    modifyResponse htmlResponse
    writeText (X.render $ (template core) pg) 

------------------------------------------------------------------------------
-- | Render JSON page

hayooJson :: Handler App App ()
hayooJson
    = do pars <- getParams
         core <- hayooCore
         modifyResponse jsonResponse
         writeText (T.pack $ evalJsonQuery (toStringMap pars) core)
    where
      toStringMap   = map (uncurry tos) . toList
          where
            tos k a   = (            T.unpack . T.decodeUtf8  $ k
                        , concatMap (T.unpack . T.decodeUtf8) $ a
                        )

evalJsonQuery :: [(String, String)] -> Core -> String
evalJsonQuery p idct
    | null request      = renderEmptyJson
    | otherwise         = renderResult
    where
    request =               getValDef p "query"  ""

    {- not used for json output

    start   = readDef 0    (getValDef p "start"  "")
    static  = readDef True (getValDef p "static" "")
    tmpl    = template idct
    -- -}

    renderResult        = renderJson
                          . either emptyRes (genResult idct)
                          . parseQuery
                          $ request

------------------------------------------------------------------------------
-- | Render HTML page

hayooHtml :: Handler App App ()
hayooHtml
    = do pars <- getParams
         core <- hayooCore
         modifyResponse htmlResponse
         writeText $ evalHtmlQuery (toStringMap pars) core
    where
      toStringMap   = map (uncurry tos) . toList
          where
            tos k a = (            T.unpack . T.decodeUtf8  $ k
                      , concatMap (T.unpack . T.decodeUtf8) $ a
                      )

evalHtmlQuery :: [(String, String)] -> Core -> T.Text
evalHtmlQuery p idct
    | null request      = renderEmptyHtml
    | otherwise         = renderResult
    where
      request =               getValDef p "query"  ""
      start   = readDef 0    (getValDef p "start"  "")
      static  = readDef True (getValDef p "static" "")
      tmpl    = template idct

      renderEmptyHtml     = X.render $ tmpl examples

      renderResult        = applyTemplate (RenderState request start static)
                            . filterStatusResult request
                            . either emptyRes (genResult idct)
                            . parseQuery
                            $ request
          where
            applyTemplate rs sr
                | rsStatic rs   = X.render $ tmpl rr
                | otherwise     = X.render $      rr
                where
                  rr            = result rs sr

------------------------------------------------------------------------------

htmlResponse :: Response -> Response
htmlResponse
    = setContentType "text/html; charset=utf-8"
      . setResponseCode 200

jsonResponse :: Response -> Response
jsonResponse
    = setContentType "application/json; charset=utf-8"
      . setResponseCode 200

hayooCore :: Handler App App HayooState
hayooCore
    = gets $ getL hayooState

------------------------------------------------------------------------------

{- snap website example stuff

setCache :: MonadSnap m => m a -> m ()
setCache act = do
    pinfo <- liftM rqPathInfo getRequest
    act
    when ("media" `B.isPrefixOf` pinfo) $ do
       expTime <- liftM (+604800) $ liftIO epochTime
       s       <- liftIO $ formatHttpTime expTime
       modifyResponse $
          setHeader "Cache-Control" "public, max-age=604800" .
          setHeader "Expires" s
-- -}

catch500 :: MonadSnap m => m a -> m ()
catch500 m = (m >> return ()) `catch` \(e::SomeException) -> do
--    let t = T.pack $ show e
    putResponse r
    writeBS "<html><head><title>Internal Server Error</title></head>"
    writeBS "<body><h1>Internal Server Error</h1>"
    writeBS "<p>A web handler threw an exception. Details:</p>"
    writeBS "<pre>\n"
--    writeText $ X.escape t
    writeBS "\n</pre></body></html>"

    logError $ B.concat [ "caught exception: ", B.pack $ show e ]
  where
    r = setContentType "text/html" $
        setResponseStatus 500 "Internal Server Error" emptyResponse


serverVersion :: SnapletSplice b v
serverVersion = liftHeist $ textSplice $ T.decodeUtf8 snapServerVersion

main :: IO ()
main = serveSnaplet defaultConfig appInit

------------------------------------------------------------------------------

ixBase          :: FilePath
ixBase          = "./lib"

getHayooInitialState    :: MonadIO m => m Core
getHayooInitialState
    = liftIO $
      do idx  <- loadIndex     hayooIndex
         infoM "Hayoo.Main" ("Hayoo index   loaded from file " ++ show hayooIndex)
               
         doc  <- loadDocuments hayooDocs
         infoM "Hayoo.Main" ("Hayoo docs    loaded from file " ++ show hayooDocs )
         infoM "Hayoo.Main" ("Hayoo docs contains " ++ show (sizeDocs doc) ++ " functions and types")

         pidx <- loadIndex     hackageIndex
         infoM "Hayoo.Main" ("Hackage index loaded from file " ++ show hackageIndex)

         pdoc <- loadPkgDocs   hackageDocs
         infoM "Hayoo.Main" ("Hackage docs  loaded from file " ++ show hackageDocs)
         infoM "Hayoo.Main" ("Hackage docs contains " ++ show (sizeDocs pdoc) ++ " packages")

         prnk <- return $ buildRankTable pdoc
         infoM "Hayoo.Main" ("Hackage package rank table computed")

         tpl  <- return $ makeTemplate (sizeDocs pdoc) (sizeDocs doc)

         return $ Core
                    { index      = idx
                    , documents  = doc
                    , pkgIndex   = pidx
                    , pkgDocs    = pdoc
                    , template   = tpl
                    , packRank   = prnk
                    }
    where
      hayooIndex      = ixBase ++ "/ix.bin.idx"
      hayooDocs       = ixBase ++ "/ix.bin.doc"
      hackageIndex    = ixBase ++ "/pkg.bin.idx"
      hackageDocs     = ixBase ++ "/pkg.bin.doc"

      infoM m msg     = hPutStrLn stderr $ m ++ ": " ++ msg

------------------------------------------------------------------------------
