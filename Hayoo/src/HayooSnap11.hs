{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- ------------------------------------------------------------

module Main where

import           Control.Applicative
import           Control.Exception       (SomeException)
import           Control.Lens
import           Control.Monad           ()
import qualified Control.Monad.CatchIO   as C (catch)
import           Control.Monad.State

import qualified Data.ByteString.Char8   as B
import           Data.Map                (toList)
import           Data.Maybe
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Data.Time.Format        (formatTime)

import           Hayoo.IndexTypes
import           Hayoo.Search.EvalSearch (emptyRes, filterStatusResult,
                                          genResult', getValDef, loadDocuments,
                                          loadIndex, loadPkgDocs, loadPkgIndex,
                                          parseQuery, readDef, renderEmptyJson,
                                          renderJson)
import           Hayoo.Search.XmlHtml    (RenderState (..), renderXmlHtml,
                                          result)

import           Heist.Interpreted

import           Holumbus.Index.Common   (sizeDocs)

import           Snap.Core
import           Snap.Http.Server
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
import           Snap.Util.GZip

import           System.Directory        (getModificationTime)
import           System.FilePath         ((</>))
import           System.Locale           (defaultTimeLocale, rfc822DateFormat)

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
      , _modTime  :: ! String
      }

makeLenses ''HayooCore

------------------------------------------------------------------------------

newtype HayooState
    = HayooState { _hayooCore :: HayooCore }

makeLenses ''HayooState

data App
    = App
      { _heist      :: Snaplet (Heist App)
      , _hayooState :: Snaplet HayooState
      }

makeLenses ''App

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
      do hs <- nestSnaplet "" heist $ heistInit "templates" -- !! templates are located in snaplets/heist/templates
         hy <- nestSnaplet "" hayooState $ hayooStateInit "./lib"

         addRoutes [ ("/hayoo/hayoo.html",    with hayooState hayooHtml)
                   , ("/hayoo/hayoo.json",    with hayooState hayooJson)
                   , ("/hayoo/examples.html", render "examples")
                   , ("/hayoo/help.html",     render "help")
                   , ("/hayoo/about.html",    render "about")
                   , ("/hayoo/api.html",      render "api")
                   , ("/hayoo/:stuff",        catch404 serveHayooStatic)
                   , ("/hayoo/",              redirect "/hayoo/hayoo.html")
                   , ("/:stuff",              redirect "/hayoo/hayoo.html")
                   , ("/",                    redirect "/hayoo/hayoo.html")
                   -- , ("/",                    writeBS "root")     -- test
                   -- , ("/:stuff",              servePong)          -- test
                   -- , ("/:stuff",              render "examples")  -- default handler
                   ]
         addSplices [ ("snap-version", serverVersion)
                    , ("packages-searched", packagesSearched $ view (snapletValue . hayooCore) hy)
                    ]
         wrapSite $ catch500 . withCompression      -- add compression and internal server error
         return $ App hs hy

packagesSearched :: HayooCore -> SnapletISplice b
packagesSearched c
    = textSplice $ T.pack $ unwords $
      [ "Concurrently search more than", show $ _sizePkg c
      , "packages and more than", show $ _sizeFct c
      , "functions!"
      , "(Index generated: ", _modTime c, ")"
      ]

------------------------------------------------------------------------------
-- | Deliver Hayoo files

serveHayooStatic :: Handler App App ()
serveHayooStatic
    = do relPath <- decodedParam "stuff"
         serveFile $ "resources/static" </> B.unpack relPath
    where
      decodedParam p = fromMaybe "" <$> getParam p
{-
servePong :: Handler App App ()
servePong
    = do relPath <- fromMaybe "???" <$> getParam "stuff"
         writeBS $ "path="
         writeBS $ relPath
--}
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
    = do s <- getSnapletState
         return $ view (snapletValue . hayooCore) s
--    = view (hayooCore) <$> getSnapletState

------------------------------------------------------------------------------

catch500 :: MonadSnap m => m a -> m ()
catch500 m
    = (m >> return ()) `C.catch`
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

catch404 :: MonadSnap m => m a -> m ()
catch404 m
    = (m >> return ()) `C.catch`
      \ (_e::SomeException) -> do
        putResponse r
        writeBS "<html><head><title>Page not found</title></head>"
        writeBS "<body><h1>Page not found</h1>"
        writeBS "</body></html>"
    where
      r = setContentType "text/html" $
          setResponseStatus 404 "Not Found" emptyResponse

serverVersion :: SnapletISplice b
serverVersion
    = textSplice $ T.decodeUtf8 snapServerVersion

------------------------------------------------------------------------------

hayooStateInit :: String -> SnapletInit App HayooState
hayooStateInit ixBase
    = makeSnaplet "hayooState" "The Hayoo! index state snaplet" Nothing $
      getHayooInitialState printInfo ixBase

getHayooInitialState    :: MonadIO m => (Text -> m ()) -> String -> m HayooState
getHayooInitialState printInfo' ixBase
    = do fidx  <- liftIO $ loadIndex     hayooIndex
         infoM "Hayoo.Main" ("Hayoo index   loaded from file " ++ show hayooIndex)

         fdoc  <- liftIO $ loadDocuments hayooDocs
         infoM "Hayoo.Main" ("Hayoo docs    loaded from file " ++ show hayooDocs )

         mtim  <- liftIO $ getModificationTime hayooIndex

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
                    , _modTime  = formatDate mtim
                    }
    where
      formatDate      = formatTime defaultTimeLocale rfc822DateFormat
      hayooIndex      = ixBase ++ "/ix.bin.idx"
      hayooDocs       = ixBase ++ "/ix.bin.doc"
      hackageIndex    = ixBase ++ "/pkg.bin.idx"
      hackageDocs     = ixBase ++ "/pkg.bin.doc"

      infoM m msg     = printInfo' $ T.pack $ m ++ ": " ++ msg

------------------------------------------------------------------------------
