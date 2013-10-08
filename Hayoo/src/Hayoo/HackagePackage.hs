module Hayoo.HackagePackage
where

import           Data.List

import           Hayoo.PackageInfo
import           Hayoo.URIConfig

import           Holumbus.Crawler.Html

import           Text.XML.HXT.Core

import           Text.Regex.XMLSchema.String

-- ------------------------------------------------------------

hayooGetPkgInfo                 :: IOSArrow XmlTree PackageInfo
hayooGetPkgInfo                 = fromLA $
                                  ( getPkgName
                                    &&&
                                    getPkgVersion
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
                                  (\ (x1, (x2, (x3, (x4, (x5, (x6, (x7, (x8, x9)))))))) -> mkPackageInfo x1 x2 x3 x4 x5 x6 x7 x8 x9)

hayooGetPkgTitle                :: IOSArrow XmlTree String
hayooGetPkgTitle                = fromLA $
                                  getPkgName

-- ------------------------------------------------------------
--
-- all the get arrows must be deterministic
-- else hayooGetPkgInfo fails, which is an error

getPkgName                      :: LA XmlTree String
getPkgName                      = getHtmlTitle
                                  >>^
                                  ( -- select the package name, the 2. word in title
                                    words
                                    >>> drop 1
                                    >>> take 1
                                    >>> unwords

                                    -- and remove trailing ":" if there is any ":"
                                    >>> reverse
                                    >>> dropColon
                                    >>> reverse
                                  )
    where
      dropColon (':' : n)       = n
      dropColon n               = n

getPkgSynopsis                  :: LA XmlTree String
getPkgSynopsis                  = getHtmlTitle
                                  >>^
                                  ( words >>> drop 2 >>> unwords )

getPkgVersion                   :: LA XmlTree String
getPkgVersion                   = getAllText
                                  $
                                  getProperty "Versions"
                                  >>>
                                  getChildren
                                  >>>
                                  hasName "strong"

getPkgDependencies              :: LA XmlTree [String]
getPkgDependencies              = ( getProperty "Dependencies"
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
                                  ) `withDefault` []

getPkgAuthor                    :: LA XmlTree String
getPkgAuthor                    = getAllText $ getProperty "Author(s)?"

getPkgMaintainer                :: LA XmlTree String
getPkgMaintainer                = getAllText $ getProperty "Maintainer(s)?"

getPkgCategory                  :: LA XmlTree String
getPkgCategory                  = getAllText $ getProperty "Category"

getPkgHomepage                  :: LA XmlTree String
getPkgHomepage                  = getAllText $ getProperty "Home page"

getPkgDescr                     :: LA XmlTree String
getPkgDescr                     = getAllText
                                  -- take all stuff between "h1" and next "h2" element in content
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
                                    (none                       -- remove h1 header
                                     `when` hasName "h1")
                                    >>>
                                    (none                       -- remove "Tags: ..." part
                                     `when` isElemWithAttr "div" "style" (== "font-size: small"))
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
