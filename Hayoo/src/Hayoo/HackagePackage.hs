module Hayoo.HackagePackage
where

import           Data.List

import           Hayoo.URIConfig
import           Hayoo.PackageInfo

import           Holumbus.Crawler.Html

import           Text.XML.HXT.Core
  
import           Text.Regex.XMLSchema.String

-- ------------------------------------------------------------

hayooGetPkgInfo                 :: IOSArrow XmlTree PackageInfo
hayooGetPkgInfo                 = fromLA $
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
                                    getPkgDescr
                                  )
                                  >>^
                                  (\ ((x1, x2), (x3, (x4, (x5, (x6, (x7, (x8, x9))))))) -> mkPackageInfo x1 x2 x3 x4 x5 x6 x7 x8 x9)

hayooGetPkgTitle                :: IOSArrow XmlTree String
hayooGetPkgTitle                = fromLA $
                                  getPkgName

-- ------------------------------------------------------------

getPkgName                      :: LA XmlTree String
getPkgName                      = getPkgNameAndVersion >>^ fst

getPkgNameAndVersion            :: LA XmlTree (String, String)
getPkgNameAndVersion            = getHtmlTitle
                                  >>^
                                  ( ( words >>> drop 1 >>> take 1 >>> unwords )
                                    >>>
                                    ( sed (const "") packageVersion''
                                      &&&
                                      ( tokenize packageVersion'' >>> reverse >>> take 1 >>> concat >>> drop 1)
                                    )
                                  )

getPkgDependencies              :: LA XmlTree [String]
getPkgDependencies              = getProperty "Dependencies"
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

getPkgAuthor                    :: LA XmlTree String
getPkgAuthor                    = getAllText $ getProperty "Author(s)?"

getPkgMaintainer                :: LA XmlTree String
getPkgMaintainer                = getAllText $ getProperty "Maintainer(s)?"

getPkgCategory                  :: LA XmlTree String
getPkgCategory                  = getAllText $ getProperty "Category"

getPkgHomepage                  :: LA XmlTree String
getPkgHomepage                  = getAllText $ getProperty "Home page"

getPkgSynopsis                  :: LA XmlTree String
getPkgSynopsis                  = ( getAllText
                                    ( getByPath ["html","body"]
                                      />
                                      isElemWithAttr "div" "id" (== "package-header")
                                      />
                                      isElemWithAttr "p" "class" (== "caption")
                                    )
                                  )
                                  >>^
                                  ( dropWhile (/= ':') >>> drop 1 >>> dropWhile (== ' '))       -- remove package name

getPkgDescr                     :: LA XmlTree String
getPkgDescr                     = getAllText                    -- take all stuff between "h1" and next "h2" element in content
                                  ( ( getByPath ["html","body"]
                                      />
                                      isElemWithAttr "div" "id" (== "content")
                                    )
                                    >>>
                                    listA getChildren
                                    >>>
                                    spanA (neg $ hasName "h2")
                                    >>>
                                    arr fst
                                    >>>
                                    unlistA
                                    >>>
                                    (none `when` hasName "h1")
                                  )

-- ------------------------------------------------------------

preparePkg                      :: IOSArrow XmlTree XmlTree
preparePkg                      = fromLA $
                                  isHackagePackage

isHackagePackage                :: LA XmlTree XmlTree
isHackagePackage                = hasAttrValue transferURI (match $ hackagePackages ++ fileName)

-- ------------------------------------------------------------

getProperties           :: LA XmlTree XmlTree
getProperties           = single (deep (hasName "table"))               -- there should be only a single table
                                                                        -- old package layout:
                                                                        -- deep ( isElemWithAttr "table" "class" (== "properties") )
                          />
                          hasName "tr"

getProperty             :: String -> LA XmlTree XmlTree
getProperty kw          = getProperties
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
