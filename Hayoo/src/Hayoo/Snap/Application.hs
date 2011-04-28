{-

This module defines our application's monad and any application-specific
information it requires.

-}

module Hayoo.Snap.Application
  ( Application
  , ApplicationState(..)
  , applicationInitializer
  ) where

import           Snap.Extension

import           Hayoo.Snap.Extension.HayooState

------------------------------------------------------------------------------
-- | 'Application' is our application's monad. It uses 'SnapExtend' from
-- 'Snap.Extension' to provide us with an extended 'MonadSnap' making use of
-- the Heist and Timer Snap extensions.
type Application = SnapExtend ApplicationState


------------------------------------------------------------------------------
-- | 'ApplicationState' is a record which contains the state needed by the Snap
-- extensions we're using.  We're using Heist so we can easily render Heist
-- templates, and Timer simply to illustrate the config loading differences
-- between development and production modes.

data ApplicationState = ApplicationState
    { hayooState    :: HayooState
    }

------------------------------------------------------------------------------

instance HasHayooState ApplicationState where
    getHayooState     = hayooState
    setHayooState s a = a { hayooState = s }


------------------------------------------------------------------------------
-- | The 'Initializer' for ApplicationState. For more on 'Initializer's, see
-- the documentation from the snap package. Briefly, this is used to
-- generate the 'ApplicationState' needed for our application and will
-- automatically generate reload\/cleanup actions for us which we don't need
-- to worry about.

applicationInitializer :: Initializer ApplicationState
applicationInitializer = do
    hayoo <- hayooInitializer
    return $ ApplicationState hayoo

------------------------------------------------------------------------------
