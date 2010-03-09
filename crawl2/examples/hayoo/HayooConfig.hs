{-# OPTIONS #-}

-- ------------------------------------------------------------

module HayooConfig
    ( hayooStart
    , hayooRefs
    , editLatestPackage
    )
where

import           Control.Applicative

import		 Data.List

import		 Holumbus.Crawler

import		 Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch	( sed )

-- ------------------------------------------------------------

hayooStart			:: [URI]
hayooStart			= hackageStart ++ gtk2hsStart

hayooRefs			:: URI -> Bool
hayooRefs			= liftA2 (||) hackageRefs gtk2hsRefs

hackageHome			:: String
hackageHome			= "http://hackage.haskell.org/packages/"

hackagePackages			:: String
hackagePackages			= "http://hackage.haskell.org/package/"		-- no "s" at the end !!!

hackageStart			:: [URI]
hackageStart			=  [ hackageHome ++ "archive/pkg-list.html" ]

hackageRefs			:: URI -> Bool
hackageRefs			= simpleFollowRef'
				  [ hackagePackages ++ packageName
                                  , packageDocPath ++ modulePath ++ ext "html"
                                  ]
                                  [ packageDocPath ++ "doc-index.*" ++ ext "html"	-- no index files
                                  , packageDocPath ++ "src/.*"				-- no hscolored sources
                                  , hackagePackages ++ packageName ++ "-[0-9.]+"	-- no package pages with (old) version numbers
                                  , rottenDocumentation
                                  ]
    where
    packageName			= fileName
    packageDocPath		= hackageHome ++ "archive/" ++ path ++ "/doc/html/"

    rottenDocumentation		= packageDocPath ++ "(" ++ intercalate "|" ds ++ ")" ++ ext "html"
        where
        ds			= [ "Database-HaskellDB-BoundedList"
                                  , "Data-TypeLevel-Num-Aliases"
                                  , "Graphics-UI-WXCore-WxcClassesAL"
                                  , "Graphics-UI-WXCore-WxcClassesMZ"
                                  , "Graphics-UI-WXCore-WxcClassTypes"
                                  , "Graphics-UI-WXCore-WxcDefs"
                                  , "Graphics-X11-Types"
                                  , "Harpy-X86Assembler"
                                  , "Types-Data-Num-Decimal-Literals"
                                  ]

gtk2hsStart			:: [URI]
gtk2hsStart			= [ gtk2hsHome ++ "index.html" ]

gtk2hsRefs			:: URI -> Bool
gtk2hsRefs			= simpleFollowRef'
				  [ gtk2hsHome ++ modulePath ++ ext "html"			-- allow html pages in current subdir
                                  ]
                                  [ path ++ "/(src/|doc-index)" ++ path				-- no source, no document index files
                                  ]

gtk2hsHome			:: String
gtk2hsHome			= "http://www.haskell.org/gtk2hs/docs/current/"

-- ------------------------------------------------------------

-- common R.E.s

moduleName			:: String
moduleName			= "[A-Z][A-Za-z0-9_]*"

modulePath			:: String
modulePath			= moduleName ++ "(-" ++ moduleName ++ ")*"

fileName			:: String
fileName			= "[^/?]+"

path				:: String
path                        	= "[^?]+"

ext				:: String -> String
ext                         	= ("[.]" ++)


-- In the package doc URIs the package version number "/*.*.*/" is substituted by the alias "/latest/"

editLatestPackage		:: String -> String
editLatestPackage		= sed (const "/latest/") "/[0-9.]+/"

-- ------------------------------------------------------------
