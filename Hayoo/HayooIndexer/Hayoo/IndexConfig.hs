{-# OPTIONS #-}

-- ------------------------------------------------------------

module Hayoo.IndexConfig
where

import           Data.Char.Properties.XMLCharProps

import           Hayoo.HackagePackage
import           Hayoo.Haddock
import           Hayoo.Signature

import           Holumbus.Crawler
import           Holumbus.Crawler.IndexerCore

import           Text.Regex.XMLSchema.String       (matchSubex)
import           Text.XML.HXT.Core

-- ------------------------------------------------------------

ix'Names        :: [String]
ix'module       :: String
ix'hierarchy    :: String
ix'package      :: String
ix'name         :: String
ix'partial      :: String
ix'signature    :: String
ix'normalized   :: String
ix'description  :: String

ix'Names@[ ix'module
         , ix'hierarchy
         , ix'package
         , ix'name
         , ix'partial
         , ix'signature
         , ix'normalized
         , ix'description
         ] = [ "module"
             , "hierarchy"
             , "package"
             , "name"
             , "partial"
             , "signature"
             , "normalized"
             , "description"
             ]


hayooIndexContextConfig         :: [IndexContextConfig]
hayooIndexContextConfig         = [ ixModule
                                  , ixHierachy
                                  , ixPackage
                                  , ixName
                                  , ixPartial
                                  , ixSignature
                                  , ixNormalizedSig
                                  , ixDescription
                                  ]
    where
    ixDefault                   = IndexContextConfig
                                  { ixc_name            = "default"
                                  , ixc_collectText     = getHtmlPlainText
                                  , ixc_textToWords     = deleteNotAllowedChars >>> words
                                  , ixc_boringWord      = boringWord
                                  }
    ixModule                    = ixDefault
                                  { ixc_name            = ix'module
                                  , ixc_collectText     = getAttrValue "module"
                                  , ixc_textToWords     = return
                                  , ixc_boringWord      = null
                                  }
    ixHierachy                  = ixModule
                                  { ixc_name            = ix'hierarchy
                                  , ixc_textToWords     = tokenize "[^.]+"              -- split module name at .
                                  }
    ixPackage                   = ixDefault
                                  { ixc_name            = ix'package
                                  , ixc_collectText     = getAttrValue "package"
                                  , ixc_textToWords     = return
                                  , ixc_boringWord      = \ x -> null x || x == "unknownpackage"
                                  }
    ixName                      = ixDefault
                                  { ixc_name            = ix'name
                                  , ixc_collectText     = getAttrValue "title"          -- is simpler than: fromLA $ getAllText (deep $ trtdWithClass (== "decl")) -- TODO 2.8 version
                                  , ixc_textToWords     = removePars                    --                  tokenize "[^ ():]+" >>> take 1
                                  , ixc_boringWord      = null                          -- (`elem` ["type", "class", "data", "module"])
                                  }
    ixPartial                   = ixName
                                  { ixc_name            = ix'partial
                                  , ixc_textToWords     = deCamel >>> tokenize typeIdent
                                  , ixc_boringWord      = boringWord
                                  }
    ixSignature                 = ixDefault
                                  { ixc_name            = ix'signature
                                  , ixc_collectText     = getAttrValue "signature"
                                  , ixc_textToWords     = stripSignature >>> return
                                  , ixc_boringWord      = not . isSignature
                                  }
    ixNormalizedSig             = ixSignature
                                  { ixc_name            = ix'normalized
                                  , ixc_textToWords     = normalizeSignature >>> return
                                  }
    ixDescription               = ixDefault
                                  { ixc_name            = ix'description
                                  , ixc_collectText     = fromLA $ getAllText hayooGetDescr
                                  , ixc_textToWords     = tokenize descrWord
                                  }

-- -----------------------------------------------------------------------------

pk'Names        :: [String]
pk'author       :: String
pk'synopsis     :: String
pk'pkgdescr     :: String
pk'dependencies :: String
pk'pkgname      :: String
pk'category     :: String

pk'Names@[ pk'category
         , pk'pkgname
         , pk'dependencies
         , pk'pkgdescr
         , pk'synopsis
         , pk'author
         ] = [ "category"
             , "pkgname"
             , "dependencies"
             , "pkgdescr"
             , "synopsis"
             , "author"
             ]

hayooPkgIndexContextConfig      :: [IndexContextConfig]
hayooPkgIndexContextConfig      = [ ixCategory
                                  , ixPkgName
                                  , ixDepends
                                  , ixDescription
                                  , ixSynopsis
                                  , ixAuthor
                                  ]
    where
    ixDefault                   = IndexContextConfig
                                  { ixc_name            = "default"
                                  , ixc_collectText     = getHtmlPlainText
                                  , ixc_textToWords     = deleteNotAllowedChars >>> words
                                  , ixc_boringWord      = boringWord
                                  }
    ixCategory                  = ixDefault
                                  { ixc_name            = pk'category
                                  , ixc_collectText     = fromLA getPkgCategory
                                  , ixc_textToWords     = words
                                  }
    ixPkgName                   = ixDefault
                                  { ixc_name            = pk'pkgname
                                  , ixc_collectText     = fromLA $ getPkgName
                                  , ixc_textToWords     = splitDash
                                  }
    ixDepends                   = ixDefault
                                  { ixc_name            = pk'dependencies
                                  , ixc_collectText     = fromLA $ getPkgDependencies >>^ unwords
                                  , ixc_textToWords     = words
                                  }
    ixDescription               = ixDefault
                                  { ixc_name            = pk'pkgdescr
                                  , ixc_collectText     = fromLA $ getPkgDescr
                                  , ixc_textToWords     = tokenize descrWord
                                  }
    ixSynopsis                  = ixDescription
                                  { ixc_name            = pk'synopsis
                                  , ixc_collectText     = fromLA $ getPkgSynopsis
                                  }
    ixAuthor                    = ixDescription
                                  { ixc_name            = pk'author
                                  , ixc_collectText     = fromLA $
                                                          listA (getPkgAuthor <+> getPkgMaintainer) >>^ unwords
                                  }

-- -----------------------------------------------------------------------------

-- please: "-" as 1. char in set !!!
-- words start with a letter, end with a letter or digit and may contain -, . and @ and digits

descrWord                       :: String
descrWord                       = "[A-Za-z][-A-Za-z0-9.@]*[A-Za-z0-9]"

-- -----------------------------------------------------------------------------

deCamel                         :: String -> String
deCamel                         = deCamel' False
    where
    deCamel' _ []               = []
    deCamel' _ ('_' : xs)       = ' ' : deCamel' True xs
    deCamel' cap (x : xs)
        | isCap x               = ( if cap
                                    then id
                                    else (' ' :)
                                  ) $
                                  x : deCamel' True      xs
        | otherwise             = x : deCamel' (isCap x) xs
        where
        isCap                   = (`elem` ['A'..'Z'])

-- -----------------------------------------------------------------------------

boringWord                      :: String -> Bool
boringWord w                    = null w
                                  ||
                                  (null . tail $ w)
                                  ||
                                  not (any isXmlLetter w)

isAllowedWordChar               :: Char -> Bool
isAllowedWordChar c             = isXmlLetter c
                                  ||
                                  isXmlDigit c
                                  ||
                                  c `elem` "_-"

deleteNotAllowedChars           :: String -> String
deleteNotAllowedChars           = map notAllowedToSpace
    where
    notAllowedToSpace c
        | isAllowedWordChar c   = c
        | otherwise             = ' '

splitDash                       :: String -> [String]
splitDash s                     = ( s : (map (\ x -> if x == '-' then ' ' else x)
                                         >>>
                                         words
                                         >>>
                                         drop 1
                                         $
                                          s
                                        )
                                  )

matchPars                       :: String -> [(String, String)]
matchPars                       = matchSubex "[(]({m}.+)[)]"

removePars                      :: String -> [String]
removePars xs
    = case matchPars xs of
        [("m", xs')]            -> [xs, xs']
        _                       -> [xs]

-- -----------------------------------------------------------------------------
