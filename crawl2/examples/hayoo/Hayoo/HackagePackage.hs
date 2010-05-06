module Hayoo.HackagePackage
where

import		 Data.List
-- import		 Data.Maybe

import           Hayoo.URIConfig
import           Hayoo.PackageInfo

import           Holumbus.Crawler.Html
-- import           Holumbus.Utility

-- import           Network.URI		( unEscapeString )

import           Text.XML.HXT.Arrow     
-- import           Text.XML.HXT.Arrow.XmlRegex
-- import           Text.XML.HXT.XPath
import		 Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch

-- ------------------------------------------------------------

hayooGetPkgInfo			:: IOSArrow XmlTree PackageInfo
hayooGetPkgInfo			= fromLA $
                                  ( getPkgNameAndVersion
				    &&&
				    getPkgDependencies
				    &&&
				    getPkgAuthor
				    &&&
				    getPkgMaintainer
				    &&&
				    getPkgCategory
				    &&&
				    getPkgHomepage
				    &&&
				    getPkgSynopsis
				    &&&
				    ( getPkgDescr >>^ limitLength 128 )
				  )
				  >>^
				  (\ ((x1, x2), (x3, (x4, (x5, (x6, (x7, (x8, x9))))))) -> mkPackageInfo x1 x2 x3 x4 x5 x6 x7 x8 x9)

hayooGetPkgTitle		:: IOSArrow XmlTree String
hayooGetPkgTitle		= fromLA $
				  getPkgName

-- ------------------------------------------------------------

getPkgName	 		:: LA XmlTree String
getPkgName			= getPkgNameAndVersion >>^ fst

getPkgNameAndVersion 		:: LA XmlTree (String, String)
getPkgNameAndVersion		= getHtmlTitle
                                  >>^
                                  ( ( words >>> drop 1 >>> take 1 >>> unwords )
                                    >>>
                                    ( sed (const "") packageVersion''
                                      &&&
                                      ( tokenize packageVersion'' >>> reverse >>> take 1 >>> concat >>> drop 1)
                                    )
                                  )

getPkgDependencies 		:: LA XmlTree [String]
getPkgDependencies		= getProperty "Dependencies"
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

getPkgAuthor 			:: LA XmlTree String
getPkgAuthor			= getAllText $ getProperty "Author(s)?"

getPkgMaintainer 		:: LA XmlTree String
getPkgMaintainer		= getAllText $ getProperty "Maintainer(s)?"

getPkgCategory 			:: LA XmlTree String
getPkgCategory			= getAllText $ getProperty "Category"

getPkgHomepage	 		:: LA XmlTree String
getPkgHomepage			= getAllText $ getProperty "Home page"

getPkgSynopsis	 		:: LA XmlTree String
getPkgSynopsis			= ( getAllText $ getByPath ["html","body", "div", "h2"] >>. take 1 )
                                  >>^
                                  ( dropWhile (/= ':') >>> drop 1 >>> dropWhile (== ' '))	-- remove package name

getPkgDescr	 		:: LA XmlTree String
getPkgDescr			= getAllText $ getByPath ["html","body", "div", "p" ] >>. take 1

-- ------------------------------------------------------------

preparePkg			:: IOSArrow XmlTree XmlTree
preparePkg			= fromLA $
                                  isHackagePackage

isHackagePackage		:: LA XmlTree XmlTree
isHackagePackage		= hasAttrValue transferURI (match $ hackagePackages ++ fileName)

-- ------------------------------------------------------------

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

-- ------------------------------------------------------------
