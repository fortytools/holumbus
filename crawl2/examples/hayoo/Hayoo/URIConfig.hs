{-# OPTIONS #-}

-- ------------------------------------------------------------

module Hayoo.URIConfig
{-
    ( hayooStart
    , hayooRefs
    , hayooGetPackage
    , editLatestPackage
    )
-}
where

import           Control.Applicative

import		 Data.List

import		 Holumbus.Crawler

import		 Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch	( sed )

-- ------------------------------------------------------------

hayooStart			:: [URI]
hayooStart			= hackageStart ++ gtk2hsStart

hayooRefs			:: [String] -> URI -> Bool
hayooRefs pkgs 			= liftA2 (||) hackageRefs' gtk2hsRefs'
    where
    hackageRefs'
	| null pkgs		= hackageRefs pkgs
	| null hackagePkgs	= const False
	| otherwise		= hackageRefs hackagePkgs
	where
	hackagePkgs		= filter (`notElem` ["gtk2hs"]) $ pkgs
    gtk2hsRefs'
	| null pkgs		= gtk2hsRefs
	| null gtk2hsPkgs	= const False
	| otherwise		= gtk2hsRefs
	where
	gtk2hsPkgs		= filter (`elem` ["gtk2hs"]) $ pkgs

hayooGetPackage			:: String -> String
hayooGetPackage u
    | not . null $ hpn		= hpn
    | not . null $ gpn		= gpn
    | otherwise			= ""
    where
    hpn				= hackageGetPackage u
    gpn				= gtk2hsGetPackage  u

hackageHome			:: String
hackageHome			= "http://hackage.haskell.org/packages/"

hackagePackages			:: String
hackagePackages			= "http://hackage.haskell.org/package/"		-- no "s" at the end !!!

hackageStart			:: [URI]
hackageStart			=  [ hackageHome ++ "archive/pkg-list.html" ]

hackageRefs			:: [String] -> URI -> Bool
hackageRefs pkgs		= simpleFollowRef'
				  [ hackagePackages ++ packageName
                                  , packageDocPath  ++ modulePath ++ ext "html"
                                  ]
                                  [ packageDocPath ++ alternatives
                                                       [ "doc-index.*" ++ ext "html"		-- no index files
                                                       , "src/.*"				-- no hscolored sources
                                                       ]
                                  , hackagePackages ++ packageName ++ "-" ++ packageVersion	-- no package pages with (old) version numbers
                                  , rottenDocumentation
                                  ]
    where
    packageVersion		= "[0-9]+([.][0-9])+"
    packageVersion'		= alternatives [packageVersion, "latest"]
    packageDocPath		= hackagePackageDocPath ++ packageName ++ "/" ++ packageVersion' ++ "/doc/html/"

    packageName
	| null pkgs		= fileName
	| otherwise		= alternatives pkgs

    rottenDocumentation		= packageDocPath ++ alternatives ds ++ ext "html"
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

hackagePackageDocPath		:: String
hackagePackageDocPath		= hackageHome ++ "archive/"

hackageGetPackage		:: String -> String
hackageGetPackage u
    | hackagePackageDocPath `isPrefixOf` u
				= takeWhile (/= '/') . drop (length hackagePackageDocPath) $ u
    | otherwise			= ""

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

gtk2hsGetPackage		:: String -> String
gtk2hsGetPackage u
    | gtk2hsHome `isPrefixOf` u	= "gtk2hs"
    | otherwise			= ""

-- ------------------------------------------------------------

-- common R.E.s

alternatives			:: [String] -> String
alternatives			= ("(" ++) . (++ ")") . intercalate "|"

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

-- ------------------------------------------------------------

-- In the package doc URIs the package version number "/*.*.*/" is substituted by the alias "/latest/"

editLatestPackage		:: String -> String
editLatestPackage		= sed (const "/latest/") "/[0-9.]+/"

-- ------------------------------------------------------------
