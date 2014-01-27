{-# LANGUAGE OverloadedStrings #-}

{-|

'Snap.Extension.Timer.Impl' is an implementation of the 'MonadTimer'
interface defined in 'Snap.Extension.Timer'.

As always, to use, add 'TimerState' to your application's state, along with an
instance of 'HasTimerState' for your application's state, making sure to use a
'timerInitializer' in your application's 'Initializer', and then you're ready to go.

This implementation does not require that your application's monad implement
interfaces from any other Snap Extension.

-}

module Hayoo.Snap.Extension.HayooState
  ( HayooState(..)
  , HasHayooState(..)
  , MonadHayoo(..)
  , hayooInitializer
  ) where

import Control.Monad.Reader

import Hayoo.IndexTypes                 ( buildRankTable )
import Hayoo.Search.EvalSearch
import Hayoo.Search.Pages.Template      ( makeTemplate )

import Holumbus.Index.Common

import Snap.Extension
import Snap.Types

import System.IO                        ( stderr
                                        , hPutStrLn
                                        )

------------------------------------------------------------------------------
-- | Your application's state must include a 'TimerState' in order for your
-- application to be a 'MonadTimer'.

newtype HayooState = HayooState
    { getCore :: Core
    }

------------------------------------------------------------------------------
-- | For your application's monad to be a 'MonadTimer', your application's
-- state needs to be an instance of 'HasTimerState'. Minimal complete
-- definition: 'getTimerState', 'setTimerState'.

class HasHayooState s where
    getHayooState :: s -> HayooState
    setHayooState :: HayooState -> s -> s


------------------------------------------------------------------------------
-- | The 'MonadHayoo' type class. Minimal complete definition: 'hayooCore'.

class MonadSnap m => MonadHayoo m where
    -- | The core Hayoo state which was last loaded.
    hayooCore :: m Core

------------------------------------------------------------------------------

instance HasHayooState s => MonadHayoo (SnapExtend s) where
    hayooCore = fmap getCore $ asks getHayooState

------------------------------------------------------------------------------

instance (MonadSnap m, HasHayooState s) => MonadHayoo (ReaderT s m) where
    hayooCore = fmap getCore $ asks getHayooState

------------------------------------------------------------------------------

instance InitializerState HayooState where
    extensionId = const "Hayoo/HayooState"
    mkCleanup   = const $ return ()
    mkReload    = const $ return ()

------------------------------------------------------------------------------
-- | The Initializer for 'HayooState'. No arguments are required.

hayooInitializer :: Initializer HayooState
hayooInitializer = liftIO getHayooInitialState >>= mkInitializer . HayooState

------------------------------------------------------------------------------

ixBase          :: FilePath
ixBase          = "./lib"

getHayooInitialState    :: IO Core
getHayooInitialState
  = do
    idx  <- loadIndex     hayooIndex
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


