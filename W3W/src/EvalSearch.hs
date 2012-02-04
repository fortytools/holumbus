-- ----------------------------------------------------------------------------

{- |
  Module     : EvalSearch

  Maintainer : Thorben Guelck, Tobias Lueders, Mathias Leonhardt, Uwe Schmidt
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Defines the Core-data-type for representing the index.
  Used by Site.hs and W3WState.hs.
-}

-- ----------------------------------------------------------------------------

module EvalSearch
    ( Core(..)
    )
where

import IndexTypes

-- ------------------------------------------------------------
-- | datatype representing the index data

data Core = Core
          { index     :: ! CompactInverted
          , documents :: ! (SmallDocuments PageInfo)
          }

-- ------------------------------------------------------------




