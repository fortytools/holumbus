{-# OPTIONS #-}

-- ------------------------------------------------------------

module Hayoo.URIConfig
    ( hayooStart
    , hayooRefs
    , hayooGetPackage
    , hayooPackageNames

    , hackagePackages
    , hackageStart
    , isHaddockURI

    , editLatestPackage

    , packageVersion
    , packageVersion'
    , packageVersion''

    , fileName
    )
where

-- import           Control.Applicative

import		 Data.List

import		 Holumbus.Crawler

import		 Text.XML.HXT.Core

-- ------------------------------------------------------------

hayooStart			:: [URI]
hayooStart			= hackageStart

hayooRefs			:: Bool -> [String] -> URI -> Bool
hayooRefs			= hackageRefs

hayooGetPackage			:: String -> String
hayooGetPackage			= hackageGetPackage

hackageHome			:: String
hackageHome			= "http://hackage.haskell.org/packages/"

hackagePackages			:: String
hackagePackages			= "http://hackage.haskell.org/package/"		-- no "s" at the end !!!

hackageStartPage		:: URI
hackageStartPage		=  hackageHome ++ "archive/pkg-list.html"

hackageStart			:: [URI]
hackageStart			=  [ hackageStartPage ]

hackageRefs			:: Bool -> [String] -> URI -> Bool
hackageRefs withDoc pkgs	= simpleFollowRef'
				  ( (hackagePackages ++ packageName')
                                    : ( if withDoc
                                        then [ packageDocPath  ++ modulePath ++ ext "html" ]
                                        else []
                                      )
                                  )
                                  [ packageDocPath ++ alternatives
                                                       [ "doc-index.*" ++ ext "html"		-- no index files
                                                       , "src/.*"				-- no hscolored sources
                                                       ]
                                  , hackagePackages ++ packageName' ++ packageVersion''		-- no package pages with (old) version numbers
                                  ]
    where
    packageDocPath		= hackagePackageDocPath ++ packageName' ++ "/" ++ packageVersion' ++ "/doc/html/"

    packageName'
	| null pkgs		= fileName
	| otherwise		= alternatives pkgs

hackagePackageDocPath		:: String
hackagePackageDocPath		= hackageHome ++ "archive/"

hackageGetPackage		:: String -> String
hackageGetPackage u
    | hackagePackageDocPath `isPrefixOf` u
				= takeWhile (/= '/') . drop (length hackagePackageDocPath) $ u
    | otherwise			= ""

getHackagePackage		:: String -> String
getHackagePackage s
    | match (hackagePackages ++ packageName) s
				= drop (length hackagePackages) s
    | otherwise			= ""

isHaddockURI			:: URI -> Bool
isHaddockURI			= match (hackagePackageDocPath ++ fileName ++ "/.+/doc/html/.+[.]html")

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

ext				:: String -> String
ext                         	= ("[.]" ++)

packageName			:: String
packageName			= "[A-Za-z]([A-Za-z0-9_]|-)*"

packageVersion, packageVersion', packageVersion''	:: String

packageVersion			= "[0-9]+([.][0-9]+)*"		-- there are package versions without sub version no
packageVersion''		= "-" ++ packageVersion
packageVersion'			= alternatives [packageVersion, "latest"]

-- ------------------------------------------------------------

-- In the package doc URIs the package version number "/*.*.*/" is substituted by the alias "/latest/"

editLatestPackage		:: String -> String
editLatestPackage		= sed (const "/latest/") "/[0-9.]+/"

-- ------------------------------------------------------------

hayooPackageNames		:: SysConfigList -> IOSArrow b String
hayooPackageNames crawlPars	= ( ( readDocument crawlPars hackageStartPage
				      >>>
				      getHtmlReferences
				      >>>
				      arr getHackagePackage
				      >>>
				      isA (not . null)
				    )
{- gtk is now on hackage
				    <+>
				    constA gtk2hsPackage
-}
				  )
				  >>. (sort >>> nub)

-- ------------------------------------------------------------
{- gtk is now on hackage

path				:: String
path                        	= "[^?]+"

gtk2hsStart			:: [URI]
gtk2hsStart			= [ gtk2hsHome ++ "index.html" ]

gtk2hsRefs			:: URI -> Bool
gtk2hsRefs			= simpleFollowRef'
				  [ gtk2hsHome ++ modulePath ++ ext "html"			-- allow html pages in current subdir
                                  ]
                                  [ path ++ "/(src/|doc-index)" ++ path				-- no source, no document index files
                                  ]

gtk2hsHome			:: String
gtk2hsHome			= "http://www.haskell.org/" ++ gtk2hsPackage ++ "/docs/current/"

gtk2hsGetPackage		:: String -> String
gtk2hsGetPackage u
    | gtk2hsHome `isPrefixOf` u	= gtk2hsPackage
    | otherwise			= ""

gtk2hsPackage			:: String
gtk2hsPackage			= "gtk2hs"


hayooStart			:: [URI]
hayooStart			= hackageStart
                                  ++
                                  gtk2hsStart

hayooRefs			:: Bool -> [String] -> URI -> Bool
hayooRefs withDoc pkgs		= liftA2 (||) hackageRefs' gtk2hsRefs'
    where
    hackageRefs'
	| null pkgs		= hackageRefs withDoc pkgs
	| null hackagePkgs	= const False
	| otherwise		= hackageRefs withDoc hackagePkgs
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
-}

-- ------------------------------------------------------------

