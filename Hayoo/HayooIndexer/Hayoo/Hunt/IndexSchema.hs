{-# LANGUAGE OverloadedStrings #-}

module Hayoo.Hunt.IndexSchema
where

import           Control.Monad.IO.Class

import           Data.Text                (Text)

import           Hunt.Common.BasicTypes
import           Hunt.Index.Schema
import           Hunt.Interpreter.Command

import           Hayoo.Hunt.Output

-- ------------------------------------------------------------

-- the context names

c'author, c'category, c'dependencies, c'description, c'homepage,
  c'maintainer, c'module, c'name, c'package, c'signature, c'source,
  c'synopsis, c'type, c'version :: Text

c'author       = "author"
c'category     = "category"
c'dependencies = "dependencies"
c'description  = "description"
c'homepage     = "homepage"
c'maintainer   = "maintainer"
c'module       = "module"
c'name         = "name"
c'package      = "package"
c'signature    = "signature"
c'source       = "source"
c'synopsis     = "synopsis"
c'type         = "type"
c'version      = "version"

-- the descripion keys, most correspond 1-1 to context names

d'author, d'category, d'dependencies, d'description, d'homepage,
  d'maintainer, d'module, d'name, d'package, d'signature, d'source,
  d'synopsis, d'type, d'version, d'rank :: Text

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
d'version      = c'version

createHayooIndexSchema :: Command
createHayooIndexSchema
    = Sequence $
      map ($ ds)
      [ mkIC c'author       . weight 1.0 . re "[^,]*"
      , mkIC c'category     . weight 1.0              . noDefault
      , mkIC c'dependencies . weight 1.0 . re "[^ ]*" . noDefault
      , mkIC c'description  . weight 0.3
      , mkIC c'homepage     . weight 1.0 . re ".*"    . noDefault
      , mkIC c'maintainer   . weight 1.0              . noDefault
      , mkIC c'module       . weight 0.5 . re ".*"
      , mkIC c'name         . weight 3.0 . re "[^ ]*"
      , mkIC c'package      . weight 1.0 . re ".*"
      , mkIC c'signature    . weight 0.2 . re ".*"
      , mkIC c'source       . weight 0.1 . re ".*"    . noDefault
      , mkIC c'synopsis     . weight 0.8
      , mkIC c'type         . weight 0.0              . noDefault
      , mkIC c'version      . weight 1.0 . re ".*"    . noDefault
      ]

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

weight :: CWeight -> ContextSchema -> ContextSchema
weight w s = s {cxWeight = w}

noDefault :: ContextSchema -> ContextSchema
noDefault s = s {cxDefault = False}

re :: CRegex -> ContextSchema -> ContextSchema
re ex s = s {cxRegEx = Just ex}

-- ------------------------------------------------------------
