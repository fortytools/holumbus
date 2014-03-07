{-# LANGUAGE OverloadedStrings #-}

module Hayoo.Hunt.IndexSchema
where

import           Control.Monad.IO.Class

import           Data.Text                (Text, pack)

import           Hayoo.Hunt.Output
import           Hayoo.IndexConfig

import           Hunt.Common.BasicTypes
import           Hunt.Index.Schema
import           Hunt.Interpreter.Command

-- ------------------------------------------------------------

-- the context names

c'author, c'category, c'dependencies, c'description, c'hierarchy, c'homepage,
  c'maintainer, c'module, c'name, c'normalized,
  c'partial, c'package, c'signature, c'source,
  c'synopsis, c'type, c'upload, c'version :: Text

c'author       = "author"
c'category     = "category"
c'dependencies = "dependencies"
c'description  = "description"
c'hierarchy    = "hierarchy"
c'homepage     = "homepage"
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

d'author, d'category, d'dependencies, d'description, d'homepage,
  d'maintainer, d'module, d'name, d'package, d'signature, d'source,
  d'synopsis, d'type, d'upload, d'uris, d'version, d'rank :: Text

d'author       = c'author
d'category     = c'category
d'dependencies = c'dependencies
d'description  = c'description
d'homepage     = c'homepage
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
    = Sequence $
      map ($ ds)
      [ mkIC c'author       . weight 1.0 . re "[^,]*"
      , mkIC c'category     . weight 1.0              . noDefault
      , mkIC c'dependencies . weight 1.0 . re "[^ ]*" . noDefault
      , mkIC c'description  . weight 0.3
      , mkIC c'hierarchy    . weight 0.5 . re "[^ ]*" . noDefault
      , mkIC c'homepage     . weight 1.0 . re ".*"    . noDefault
      , mkIC c'maintainer   . weight 1.0              . noDefault
      , mkIC c'module       . weight 0.5 . re ".*"
      , mkIC c'name         . weight 3.0 . re "[^ ]*"
      , mkIC c'normalized   . weight 0.2 . re ".*"
      , mkIC c'package      . weight 1.0 . re ".*"
      , mkIC c'partial      . weight 1.0 . re "[^ ]*"
      , mkIC c'signature    . weight 0.2 . re ".*"
      , mkIC c'source       . weight 0.1 . re ".*"    . noDefault
      , mkIC c'synopsis     . weight 0.8
      , mkIC c'type         . weight 0.0              . noDefault
      , mkIC c'upload       . weight 1.0 . re dr      . noDefault . datecx
      , mkIC c'version      . weight 1.0 . re ".*"    . noDefault
      ]
    where
      d4 = "[0-9]{4}"
      d2 = "[0-9]{2}"
      ms = "-"
      cl = ":"
      dr = pack $ concat [d4, "(", ms, d2, "(", ms, d2, "(T", d2, cl, d2, cl, d2,")?)?)?"]

dropHayooIndexSchema :: Command
dropHayooIndexSchema
    = Sequence . map (DeleteContext . icICon) . icCmdSeq $ createHayooIndexSchema


execCreateHayooIndexSchema :: MonadIO m => Maybe String -> m ()
execCreateHayooIndexSchema target
    = outputValue (maybe (Left "00-schema") Right target) createHayooIndexSchema

execDropHayooIndexSchema :: MonadIO m => Maybe String -> m ()
execDropHayooIndexSchema target
    = outputValue (maybe (Left "00-delete-schema") Right target) dropHayooIndexSchema

-- ------------------------------------------------------------

mkIC :: Context -> ContextSchema -> Command
mkIC name schema
    = InsertContext
      { icICon   = name
      , icSchema = schema
      }

ds :: ContextSchema
ds  = ContextSchema
      { cxRegEx      = Just "\\w*"
      , cxNormalizer = []
      , cxWeight     = 1.0
      , cxDefault    = True
      , cxType       = ctText
      }

datecx :: ContextSchema -> ContextSchema
datecx s = s {cxType = ctDate}

weight :: CWeight -> ContextSchema -> ContextSchema
weight w s = s {cxWeight = w}

noDefault :: ContextSchema -> ContextSchema
noDefault s = s {cxDefault = False}

re :: CRegex -> ContextSchema -> ContextSchema
re ex s = s {cxRegEx = Just ex}

-- ------------------------------------------------------------
