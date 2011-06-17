{-

This module defines our application's monad and any application-specific
information it requires.

-}

module Application ( Application, applicationInitializer )

where

import Snap.Extension
import Snap.Extension.Heist.Impl
import W3WState

------------------------------------------------------------------------------

-- 'Application' is our application's monad. It uses 'SnapExtend' from
-- 'Snap.Extension' to provide us with an extended 'MonadSnap' making use of
-- the Heist extension.
type Application = SnapExtend ApplicationState

------------------------------------------------------------------------------

-- 'ApplicationState' is a record which contains the state needed by the Snap
-- extension we're using.  We're using Heist so we can easily render Heist
-- templates.

data ApplicationState = ApplicationState
  {
    templateState :: HeistState Application,
  	w3wState      :: W3WState  
  }

------------------------------------------------------------------------------

instance HasHeistState Application ApplicationState where
	getHeistState     = templateState
	setHeistState s a = a { templateState = s }


instance HasW3WState ApplicationState where
    getW3WState     = w3wState
    setW3WState s a = a { w3wState = s }

------------------------------------------------------------------------------

-- The 'Initializer' for ApplicationState. This is used to generate the 
-- 'ApplicationState' needed for our application and will automatically
-- generate reload\/cleanup actions for us.

applicationInitializer :: Initializer ApplicationState
applicationInitializer = do
    heist <- heistInitializer "resources/templates"
    w3w <- w3wInitializer
    return $ ApplicationState heist w3w

