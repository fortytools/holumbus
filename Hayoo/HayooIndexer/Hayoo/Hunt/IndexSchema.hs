{-# LANGUAGE OverloadedStrings #-}

module Hayoo.Hunt.IndexSchema
where

import           Control.Applicative    ()
import           Control.Monad.IO.Class

import           Data.Text              (Text, pack, unpack)
import           Data.Time

import           Hayoo.Hunt.Output
import           Hayoo.IndexConfig

import           Hunt.ClientInterface

import           System.Locale          (defaultTimeLocale)

-- ------------------------------------------------------------

-- the context names

c'author, c'category, c'dependencies, c'description, c'hierarchy, c'homepage,
  c'indexed, c'maintainer, c'module, c'name, c'normalized,
  c'partial, c'package, c'signature, c'source,
  c'synopsis, c'type, c'upload, c'version :: Text

c'author       = "author"
c'category     = "category"
c'dependencies = "dependencies"
c'description  = "description"
c'hierarchy    = "hierarchy"
c'homepage     = "homepage"
c'indexed      = "indexed"
c'maintainer   = "maintainer"
c'module       = "module"
c'name         = "name"
c'normalized   = "normalized"
c'package      = "package"
c'partial      = "partial"
c'signature    = "signature"
c'source       = "source"
c'synopsis     = "synopsis"
c'type         = "type"
c'upload       = "upload"
c'version      = "version"

cxToHuntCx :: String -> Text
cxToHuntCx cx
    = maybe (error $ "no Hunt context found for: " ++ show cx) id . lookup cx $
      [ (ix'description,  c'description)        -- the haddock contexts
      , (ix'hierarchy,    c'hierarchy)
      , (ix'module,       c'module)
      , (ix'name,         c'name)
      , (ix'normalized,   c'normalized)
      , (ix'package,      c'package)
      , (ix'partial,      c'partial)
      , (ix'signature,    c'signature)

      , (pk'author,       c'author)             -- the hackage package contexts
      , (pk'category,     c'category)
      , (pk'dependencies, c'dependencies)
      , (pk'pkgdescr,     c'description)
      , (pk'pkgname,      c'name)
      , (pk'synopsis,     c'synopsis)
      ]

-- the descripion keys, most correspond 1-1 to context names

d'author, d'category, d'dependencies, d'description, d'homepage, d'indexed,
  d'maintainer, d'module, d'name, d'package, d'signature, d'source,
  d'synopsis, d'type, d'upload, d'uris, d'version, d'rank :: Text

d'author       = c'author
d'category     = c'category
d'dependencies = c'dependencies
d'description  = c'description
d'homepage     = c'homepage
d'indexed      = c'indexed
d'maintainer   = c'maintainer
d'module       = c'module
d'name         = c'name
d'package      = c'package
d'rank         = "rank"
d'signature    = c'signature
d'source       = c'source
d'synopsis     = c'synopsis
d'type         = c'type
d'upload       = c'upload
d'uris         = "uris"
d'version      = c'version

createHayooIndexSchema :: Command
createHayooIndexSchema
    = cmdSequence $
      map (uncurry cmdInsertContext) $
      hayooIndexSchema

dropHayooIndexSchema :: Command
dropHayooIndexSchema
    = cmdSequence $
      map (cmdDeleteContext . fst) $
      hayooIndexSchema

hayooIndexSchema :: [(Context, ContextSchema)]
hayooIndexSchema
    = map ($ ds) $
      [ mkIC c'author       . withCxWeight 1.0
      , mkIC c'category     . withCxWeight 1.0                       . withoutCxDefault
      , mkIC c'dependencies . withCxWeight 1.0 . withCxRegEx "[^ ]*" . withoutCxDefault
      , mkIC c'description  . withCxWeight 0.3
      , mkIC c'hierarchy    . withCxWeight 0.1
      , mkIC c'indexed      . withCxWeight 1.0 . withCxRegEx dr      . withoutCxDefault . withCxDate
      , mkIC c'maintainer   . withCxWeight 1.0                       . withoutCxDefault
      , mkIC c'module       . withCxWeight 0.5 . withCxRegEx ".*"
      , mkIC c'name         . withCxWeight 3.0 . withCxRegEx "[^ ]*"
      , mkIC c'normalized   . withCxWeight 0.2 . withCxRegEx "[^$]*" . withoutCxDefault -- from Hayoo.ParseSignature.modifySignatureWith
      , mkIC c'package      . withCxWeight 1.0 . withCxRegEx ".*"
      , mkIC c'partial      . withCxWeight 0.2 . withCxRegEx "[^ ]*"
      , mkIC c'signature    . withCxWeight 1.0 . withCxRegEx "[^$]*" . withoutCxDefault -- from Hayoo.ParseSignature.modifySignatureWith
      , mkIC c'source       . withCxWeight 0.1 . withCxRegEx ".*"    . withoutCxDefault
      , mkIC c'synopsis     . withCxWeight 0.8
      , mkIC c'type         . withCxWeight 0.0                       . withoutCxDefault
      , mkIC c'upload       . withCxWeight 1.0 . withCxRegEx dr      . withoutCxDefault . withCxDate
      , mkIC c'version      . withCxWeight 1.0 . withCxRegEx ".*"    . withoutCxDefault
      ]
    where
      mkIC x cs = (x, cs)
      d4 = "[0-9]{4}"
      d2 = "[0-9]{2}"
      ms = "-"
      cl = ":"
      dr = pack $ concat [d4, "(", ms, d2, "(", ms, d2, "(T", d2, cl, d2, cl, d2,")?)?)?"]

execCreateHayooIndexSchema :: (Functor m, MonadIO m) => Maybe String -> m ()
execCreateHayooIndexSchema target
    = outputValue (maybe (Left "00-schema") Right target) createHayooIndexSchema
      >>= evalOkRes

execDropHayooIndexSchema :: (Functor m, MonadIO m) => Maybe String -> m ()
execDropHayooIndexSchema target
    = outputValue (maybe (Left "00-delete-schema") Right target) dropHayooIndexSchema
      >>= evalOkRes

-- ------------------------------------------------------------

ds :: ContextSchema
ds  = withCxRegEx "\\w*" $ mkSchema

fmtDateXmlSchema :: UTCTime -> Text
fmtDateXmlSchema = fmtDate' "%FT%X"

fmtDateHTTP :: UTCTime -> Text
fmtDateHTTP = fmtDate' "%a %b %e %H:%M:%S %Z %Y"

fmtDate' :: String -> UTCTime -> Text
fmtDate' fmt
    = pack . formatTime defaultTimeLocale fmt

parseDateHTTP :: String -> Maybe UTCTime
parseDateHTTP = parseTime defaultTimeLocale "%a %b %e %H:%M:%S %Z %Y"

mkSaveCmd :: UTCTime -> Command
mkSaveCmd now = cmdStoreIndex fn
          where
            fn = "hayoo-ix." ++ (unpack . fmtDateXmlSchema $ now)

appendSaveCmd :: Bool -> UTCTime -> Command -> Command
appendSaveCmd True now cmd
    = cmdSequence [cmd, mkSaveCmd now]
appendSaveCmd False _ cmd
    = cmd

-- ------------------------------------------------------------
