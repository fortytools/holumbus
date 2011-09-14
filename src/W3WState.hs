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

module W3WState
  ( W3WState(..)
  , HasW3WState(..)
  , MonadW3W(..)
  , w3wInitializer
  ) where

import Control.Monad.Reader
import EvalSearch
import Holumbus.Index.Common
import W3WSimpleSearch
import Snap.Extension
import Snap.Types
import System.IO	( stderr
                    , hPutStrLn
                    )

------------------------------------------------------------------------------
-- | Your application's state must include a 'TimerState' in order for your
-- application to be a 'MonadTimer'.

newtype W3WState = W3WState
    {
		getCore :: Core
    }

------------------------------------------------------------------------------
-- | For your application's monad to be a 'MonadTimer', your application's
-- state needs to be an instance of 'HasTimerState'. Minimal complete
-- definition: 'getTimerState', 'setTimerState'.

class HasW3WState s where
    getW3WState :: s -> W3WState
    setW3WState :: W3WState -> s -> s


------------------------------------------------------------------------------
-- | The 'MonadW3W' type class. Minimal complete definition: 'w3wCore'.

class MonadSnap m => MonadW3W m where
    -- | The core W3W state which was last loaded.
    w3wCore :: m Core

------------------------------------------------------------------------------

instance HasW3WState s => MonadW3W (SnapExtend s) where
    w3wCore = fmap getCore $ asks getW3WState

------------------------------------------------------------------------------

instance (MonadSnap m, HasW3WState s) => MonadW3W (ReaderT s m) where
    w3wCore = fmap getCore $ asks getW3WState

------------------------------------------------------------------------------

instance InitializerState W3WState where
    extensionId = const "W3W/W3WState"
    mkCleanup   = const $ return ()
    mkReload    = const $ return ()

------------------------------------------------------------------------------
-- | The Initializer for 'W3WState'. No arguments are required.

w3wInitializer :: Initializer W3WState
w3wInitializer = liftIO getW3WInitialState >>= mkInitializer . W3WState

------------------------------------------------------------------------------

ixBase          :: FilePath
ixBase          = "./index"

getW3WInitialState    :: IO Core
getW3WInitialState = do
	idx  <- loadIndex w3wIndex
	infoM "W3W.Main" ("W3W index   loaded from file " ++ show w3wIndex)
	doc  <- loadDocuments w3wDocs
	infoM "W3W.Main" ("W3W docs    loaded from file " ++ show w3wDocs )
	infoM "W3W.Main" ("W3W docs contains " ++ show (sizeDocs doc) ++ " entries")
	return $ Core
             { index      = idx
             , documents  = doc
             }
		where
			w3wIndex      = ixBase ++ "/ix.bin.idx"
			w3wDocs       = ixBase ++ "/ix.bin.doc"
			infoM m msg   = hPutStrLn stderr $ m ++ ": " ++ msg

