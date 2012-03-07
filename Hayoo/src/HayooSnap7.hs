{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- ------------------------------------------------------------

module Main where

import           Control.Category               ( (>>>) )
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

import           Data.Text                      ( Text )
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T

import           Hayoo.IndexTypes
import           Hayoo.Search.EvalSearch        ( emptyRes
                                                , filterStatusResult
                                                , genResult'
                                                , getValDef
                                                , loadDocuments
                                                , loadIndex
                                                , loadPkgDocs
                                                , loadPkgIndex
                                                , parseQuery
                                                , readDef
                                                , renderEmptyJson
                                                , renderJson
                                                )
import           Hayoo.Search.XmlHtml           ( RenderState(..)
                                                , result
                                                , renderXmlHtml
                                                )
import           Holumbus.Index.Common          ( sizeDocs )

import           Prelude                        hiding (catch)

import           Snap.Http.Server
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
-- {-
import           Snap.Util.GZip
-- -}
import           System.FilePath                ( (</>) )

import           Text.Templating.Heist

------------------------------------------------------------------------------

data DocIndex a
    = DocIndex
      { _docIx :: ! CompactInverted
      , _docTb :: ! (SmallDocuments a)
      }

-- makeLenses [''DocIndex]

data HayooCore
    = HayooCore
      { _fctDocIx :: ! (DocIndex FunctionInfo)
      , _pkgDocIx :: ! (DocIndex PackageInfo)
      , _sizeFct  :: ! Int
      , _sizePkg  :: ! Int
      , _pkgRank  :: ! RankTable
      }

makeLenses [''HayooCore]

------------------------------------------------------------------------------

newtype HayooState
    = HayooState { _hayooCore :: HayooCore }

makeLenses [''HayooState]

data App
    = App
      { _heist      :: Snaplet (Heist App)
      , _hayooState :: Snaplet HayooState
      }

makeLenses [''App]

instance HasHeist App where
    heistLens = subSnaplet heist

------------------------------------------------------------------------------

main :: IO ()
main = serveSnaplet defaultConfig appInit

description :: Text
description
    = "The Hayoo! Search Engine"

appInit :: SnapletInit App App
appInit
    = makeSnaplet "hayoo" description Nothing $
      do hs <- nestSnaplet "" heist $ heistInit "resources/templates"
         hy <- nestSnaplet "" hayooState $ hayooStateInit "./lib"

         addRoutes [ ("/",              render "examples")
                   , ("/hayoo.html",    with hayooState hayooHtml)
                   , ("/hayoo.json",    with hayooState hayooJson)
                   , ("/examples.html", render "examples")
                   , ("/help.html",     render "help")
                   , ("/about.html",    render "about")
                   , ("/api.html",      render "api")
                   , ("/hayoo/:stuff",  serveHayooStatic)
                   , ("/:stuff",        render "examples")  -- default handler
                   ]
         addSplices [ ("snap-version", serverVersion)
                    , ( "packages-searched"
                      , packagesSearched $
                        getL (snapletValue >>> hayooCore) hy
                      )
                    ]
         wrapHandlers $ catch500 . withCompression      -- add compression and internal server error
         return $ App hs hy

packagesSearched :: HayooCore -> SnapletSplice b v
packagesSearched c
    = liftHeist $ textSplice $ T.pack $ unwords $
      [ "Concurrently search more than", show $ _sizePkg c
      , "packages and more than", show $ _sizeFct c
      , "functions!"
      ]
      
------------------------------------------------------------------------------
-- | Deliver Hayoo files

serveHayooStatic :: Handler App App ()
serveHayooStatic
    = do relPath <- decodedParam "stuff"
         serveFile $ "resources/static" </> B.unpack relPath
    where
      decodedParam p = fromMaybe "" <$> getParam p

------------------------------------------------------------------------------
-- | Render JSON page

hayooJson :: Handler App HayooState ()
hayooJson
    = do pars <- getParams
         core <- getHayooCore
         modifyResponse jsonResponse
         writeText (T.pack $ evalJsonQuery (toStringMap pars) core)
    where
      toStringMap   = map (uncurry tos) . toList
          where
            tos k a   = (            T.unpack . T.decodeUtf8  $ k
                        , concatMap (T.unpack . T.decodeUtf8) $ a
                        )

evalJsonQuery :: [(String, String)] -> HayooCore -> String
evalJsonQuery p c
    | null request      = renderEmptyJson
    | otherwise         = renderResult
    where
      (DocIndex fx fd) = _fctDocIx c
      (DocIndex px pd) = _pkgDocIx c
      rt               = _pkgRank  c
      request          = getValDef p "query"  ""
      renderResult
          = renderJson
            . either emptyRes (\ q -> genResult' q fx fd px pd rt)
            . parseQuery
            $ request

    {- not used for json output

    start   = readDef 0    (getValDef p "start"  "")
    static  = readDef True (getValDef p "static" "")
    -- -}

------------------------------------------------------------------------------
-- | Render HTML page

hayooHtml :: Handler App HayooState ()
hayooHtml
    = do pars <- getParams
         core <- getHayooCore
         evalHtmlQuery (toStringMap pars) core
    where
      toStringMap   = map (uncurry tos) . toList
          where
            tos k a = (            T.unpack . T.decodeUtf8  $ k
                      , concatMap (T.unpack . T.decodeUtf8) $ a
                      )

evalHtmlQuery :: [(String, String)] -> HayooCore -> Handler App HayooState ()
evalHtmlQuery p c
    | null request      = renderEmptyHtml
    | otherwise         = renderResult
    where
      (DocIndex fx fd) = _fctDocIx c
      (DocIndex px pd) = _pkgDocIx c
      rt               = _pkgRank  c

      request          =               getValDef p "query"  ""
      start            = readDef 0    (getValDef p "start"  "")
      static           = readDef True (getValDef p "static" "")

      renderEmptyHtml
          = render "examples"

      renderRes tpl ns
          = heistLocal (bindSplices [("results", return ns)]) $
            render tpl

      renderResult
          = applyTemplate (RenderState request start static) $
            filterStatusResult request $
            either emptyRes (\ q -> genResult' q fx fd px pd rt) $
            parseQuery $
            request
          where
            applyTemplate rs sr
                = renderRes (if rsStatic rs then "results-stat" else "results-dyn") $
                  renderXmlHtml $
                  result rs sr

------------------------------------------------------------------------------

htmlResponse :: Response -> Response
htmlResponse
    = setContentType "text/html; charset=utf-8"
      . setResponseCode 200

jsonResponse :: Response -> Response
jsonResponse
    = setContentType "application/json; charset=utf-8"
      . setResponseCode 200

getHayooCore :: Handler App HayooState HayooCore
getHayooCore
    = getL (snapletValue >>> hayooCore) <$> getSnapletState

------------------------------------------------------------------------------

catch500 :: MonadSnap m => m a -> m ()
catch500 m
    = (m >> return ()) `catch`
      \ (e::SomeException) -> do
        --    let t = T.pack $ show e
        putResponse r
        writeBS "<html><head><title>Internal Server Error</title></head>"
        writeBS "<body><h1>Internal Server Error</h1>"
        writeBS "<p>A web handler threw an exception.</p>"
        writeBS "</body></html>"

        logError $ B.concat [ "caught exception: ", B.pack $ show e ]
    where
      r = setContentType "text/html" $
          setResponseStatus 500 "Internal Server Error" emptyResponse


serverVersion :: SnapletSplice b v
serverVersion
    = liftHeist $ textSplice $ T.decodeUtf8 snapServerVersion

------------------------------------------------------------------------------

hayooStateInit :: String -> SnapletInit App HayooState
hayooStateInit ixBase
    = makeSnaplet "hayooState" "The Hayoo! index state snaplet" Nothing $
      getHayooInitialState ixBase

getHayooInitialState    :: String -> Initializer b v HayooState
getHayooInitialState ixBase
    = do fidx  <- liftIO $ loadIndex     hayooIndex
         infoM "Hayoo.Main" ("Hayoo index   loaded from file " ++ show hayooIndex)
               
         fdoc  <- liftIO $ loadDocuments hayooDocs
         infoM "Hayoo.Main" ("Hayoo docs    loaded from file " ++ show hayooDocs )
         infoM "Hayoo.Main" ("Hayoo docs contains " ++ show (sizeDocs fdoc) ++ " functions and types")

         pidx <- liftIO $ loadPkgIndex     hackageIndex
         infoM "Hayoo.Main" ("Hackage index loaded from file " ++ show hackageIndex)

         pdoc <- liftIO $ loadPkgDocs   hackageDocs
         infoM "Hayoo.Main" ("Hackage docs  loaded from file " ++ show hackageDocs)
         infoM "Hayoo.Main" ("Hackage docs contains " ++ show (sizeDocs pdoc) ++ " packages")

         prnk <- return $ buildRankTable pdoc
         infoM "Hayoo.Main" ("Hackage package rank table computed")

         return $ HayooState $ HayooCore
                    { _fctDocIx = DocIndex fidx fdoc
                    , _pkgDocIx = DocIndex pidx pdoc
                    , _sizeFct  = sizeDocs fdoc
                    , _sizePkg  = sizeDocs pdoc
                    , _pkgRank  = prnk
                    }
    where
      hayooIndex      = ixBase ++ "/ix.bin.idx"
      hayooDocs       = ixBase ++ "/ix.bin.doc"
      hackageIndex    = ixBase ++ "/pkg.bin.idx"
      hackageDocs     = ixBase ++ "/pkg.bin.doc"

      infoM m msg     = printInfo $ T.pack $ m ++ ": " ++ msg

------------------------------------------------------------------------------
