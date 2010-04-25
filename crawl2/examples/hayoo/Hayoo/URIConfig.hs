{-# OPTIONS #-}

-- ------------------------------------------------------------

module Hayoo.URIConfig
{-
    ( hayooStart
    , hayooRefs
    , hayooGetPackage
    , hayooPackageNames
    , editLatestPackage
    )
-}
where

import           Control.Applicative

import		 Data.List

import		 Holumbus.Crawler

import		 Text.XML.HXT.Arrow		hiding ( readDocument )
import           Text.XML.HXT.Arrow.XmlCache
import		 Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch	( match, sed )

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

hackageStartPage		:: URI
hackageStartPage		=  hackageHome ++ "archive/pkg-list.html"

hackageStart			:: [URI]
hackageStart			=  [ hackageStartPage ]

hackageRefs			:: [String] -> URI -> Bool
hackageRefs pkgs		= simpleFollowRef'
				  [ hackagePackages ++ packageName'
                                  , packageDocPath  ++ modulePath ++ ext "html"
                                  ]
                                  [ packageDocPath ++ alternatives
                                                       [ "doc-index.*" ++ ext "html"		-- no index files
                                                       , "src/.*"				-- no hscolored sources
                                                       ]
                                  , hackagePackages ++ packageName' ++ "-" ++ packageVersion	-- no package pages with (old) version numbers
                                  -- , rottenDocumentation
                                  ]
    where
    packageDocPath		= hackagePackageDocPath ++ packageName' ++ "/" ++ packageVersion' ++ "/doc/html/"

    packageName'
	| null pkgs		= fileName
	| otherwise		= alternatives pkgs

{- rotten documentation is filtered out by limiting the length of the document (max 1Mb)

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
-}

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

-- ------------------------------------------------------------

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

packageName			:: String
packageName			= "[A-Za-z]([A-Za-z0-9_]|-)*"

packageVersion, packageVersion'	:: String
packageVersion			= "[0-9]+([.][0-9]+)+"
packageVersion'			= alternatives [packageVersion, "latest"]

-- ------------------------------------------------------------

-- In the package doc URIs the package version number "/*.*.*/" is substituted by the alias "/latest/"

editLatestPackage		:: String -> String
editLatestPackage		= sed (const "/latest/") "/[0-9.]+/"

-- ------------------------------------------------------------

hayooPackageNames		:: [(String, String)] -> IOSArrow b String
hayooPackageNames crawlPars	= ( ( readDocument crawlPars hackageStartPage
				      >>>
				      getHtmlReferences
				      >>>
				      arr getHackagePackage
				      >>>
				      isA (not . null)
				    )
				    <+>
				    constA gtk2hsPackage
				  )
				  >>. (sort >>> nub)

-- ------------------------------------------------------------

hayooPackageDescr		:: [String] -> [(String, String)] -> IOSArrow b (String, (String, ([String], Bool)))
hayooPackageDescr pkgList crawlPars
				= pkgDescr $< ( if null pkgList
                                                then hayooPackageNames crawlPars
                                                else constL pkgList
                                              )
    where
    pkgDescr pn			= readDocument crawlPars pkgUrl
                                  >>>
                                  ( ( documentStatusOk
                                      >>>
                                      hasAttrValue transferStatus (== "200")
                                    )
                                    `guards`
                                    fromLA
                                    ( constA pn
                                      &&&
                                      ( getVersionNo `withDefault` "unknown" )
                                      &&&
                                      ( getDepends   `withDefault` []        )
                                      &&&
                                      ( hasHaddock   `withDefault` False     )
                                    )
                                  )
        where
        pkgUrl			= hackagePackages ++ pn

        getProperties		:: LA XmlTree XmlTree
        getProperties		= deep ( isElemWithAttr "table" "class" (== "properties") )
                                  />
                                  hasName "tr"

        getProperty		:: String -> LA XmlTree XmlTree
        getProperty kw		= getProperties
                                  >>>
                                  ( ( getChildren
                                      >>>
                                      hasName "th"
                                      >>>
                                      ( getChildren >>. take 1 )
                                      >>>
                                      hasText (match kw)
                                    )
                                    `guards`
                                    ( getChildren
                                      >>>
                                      hasName "td"
                                    )
                                  )

        getVersionNo		:: LA XmlTree String
        getVersionNo		= getProperty "Version(s)?"
                                  >>>
                                  xshow
                                  ( getChildren
                                    >>>
                                    hasName "b"
                                    >>>
                                    ( getChildren >>. take 1 )
                                    >>>
                                    isText
                                  )
                                         
        getDepends		= getProperty "Dependencies"
                                  >>>
                                  listA
                                  ( getChildren
                                    >>>
                                    hasName "a"
                                    >>>
                                    getChildren
                                    >>>
                                    getText
                                    >>>
                                    this -- checkPackageName
                                  )
                                  >>>
                                  arr (sort >>> nub)

        hasHaddock		= listA
                                  ( deep ( isElemWithAttr "ul" "class" (== "modules") )
                                    />
                                    deep ( isElemWithAttr "a" "href" ( ("/packages/archive/" ++ pn ++ "/") `isPrefixOf`) )
                                  )
                                  >>^ (not . null)


-- ------------------------------------------------------------

isElemWithAttr			:: String -> String -> (String -> Bool) -> LA XmlTree XmlTree
isElemWithAttr en an av		= isElem
                                  >>>
                                  hasName en
                                  >>>
                                  hasAttrValue an av

-- ------------------------------------------------------------

