-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Distribution.Master
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.Distribution.Master
(
  MasterClass(..)
)
where

import qualified Holumbus.MapReduce.Types as T
import qualified Holumbus.MapReduce.MapReduce as MR


-- ----------------------------------------------------------------------------
-- Typeclass
-- ----------------------------------------------------------------------------


class (MR.MapReduce m) => MasterClass m where

  closeMaster :: m -> IO () 
  
  receiveTaskCompleted :: T.TaskData -> m -> IO m

  receiveTaskError :: T.TaskData -> m -> IO m