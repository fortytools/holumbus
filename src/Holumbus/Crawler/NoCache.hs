{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.NoCache
where

import		 Holumbus.Index.Common( HolCache (..))

-- ------------------------------------------------------------

data NoCache	= NoCache

emptyNoCache	:: NoCache
emptyNoCache	= NoCache

instance HolCache NoCache where
    getDocText _ _ _ 	= return Nothing
    putDocText _ _ _ _	= return ()
    mergeCaches _ _	= return NoCache

-- ------------------------------------------------------------
