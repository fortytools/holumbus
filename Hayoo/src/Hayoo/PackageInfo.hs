{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- ------------------------------------------------------------

module Hayoo.PackageInfo
where

import           Control.DeepSeq

import           Data.Binary           (Binary (..))
import qualified Data.Binary           as B
import           Data.Size
import           Data.Typeable

import           Holumbus.Query.Result (Score)

import           Text.XML.HXT.Core

-- ------------------------------------------------------------

-- | Additional information about a function.
--
-- The strings in this record are not compressed into bytestrings.
-- The document table contains bzip compressed bytestings of serialized
-- records, which are unpacked when accessing the document descriptions.
-- So there is non need for unsing bytestrings and strict fields

data PackageInfo
    = PackageInfo
      { p_name         :: String               -- ^ The name of the package
      , p_version      :: String               -- ^ The latest package version
      , p_dependencies :: String               -- ^ The list of required packages
      , p_author       :: String               -- ^ The author
      , p_maintainer   :: String               -- ^ The maintainer
      , p_category     :: String               -- ^ The package category
      , p_homepage     :: String               -- ^ The home page
      , p_synopsis     :: String               -- ^ The synopsis
      , p_description  :: String               -- ^ The description of the package
      , p_rank         :: Score                -- ^ The ranking
      }
    deriving (Show, Eq, Typeable)

mkPackageInfo                   :: String -> String -> [String] -> String -> String -> String -> String -> String -> String -> PackageInfo
mkPackageInfo n v d a m c h y s = PackageInfo n v (unwords d) a m c h y s 1.0

setPackageRank                  :: Score -> PackageInfo -> PackageInfo
setPackageRank r p              = r `seq` p { p_rank = r }

getPackageName                  :: PackageInfo -> String
getPackageName                  =  p_name

getPackageDependencies          :: PackageInfo -> [String]
getPackageDependencies          = words . p_dependencies

instance XmlPickler PackageInfo where
    xpickle                     = xpWrap (fromTuple, toTuple) xpPackage
        where
        fromTuple ((n, v, d), a, m, c, h, (y, s, r))
                                = PackageInfo n v d a m c h y s r
        toTuple (PackageInfo n v d a m c h y s r)
                                = ((n, v, d)
                                  , a, m, c, h
                                  ,(y, s, r)
                                  )
        xpPackage               = xp6Tuple
                                  (xpTriple xpName xpVersion xpDependencies)
                                  xpAuthor xpMaintainer xpCategory xpHomepage
                                  (xpTriple xpSynopsis xpDescr xpRank)
            where
            xpName              = xpAttr "name"         xpText0
            xpVersion           = xpAttr "version"      xpText0
            xpDependencies      = xpAttr "dependencies" xpText0
            xpAuthor            = xpAttr "author"       xpText0
            xpMaintainer        = xpAttr "maintainer"   xpText0
            xpCategory          = xpAttr "category"     xpText0
            xpHomepage          = xpAttr "homepage"     xpText0
            xpSynopsis          = xpAttr "synopsis"     xpText0
            xpDescr             = xpText
            xpRank              = xpAttr "rank" $
                                  xpWrap (read, show)   xpText0

instance NFData PackageInfo where
  rnf (PackageInfo n v d a m c h y s r)
                                = rnf n `seq` rnf v `seq` rnf d `seq` rnf a `seq`
                                  rnf m `seq` rnf c `seq` rnf h `seq` rnf y `seq`
                                  rnf s `seq` r          `seq` ()

instance B.Binary PackageInfo where
    put (PackageInfo x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)
                                = put x1 >> put x2 >> put x3 >> put x4 >> put x5 >>
                                  put x6 >> put x7 >> put x8 >> put x9 >> put x10
    get                         = do
                                  x1 <- get
                                  x2 <- get
                                  x3 <- get
                                  x4 <- get
                                  x5 <- get
                                  x6 <- get
                                  x7 <- get
                                  x8 <- get
                                  x9 <- get
                                  x10 <- get
                                  let r = PackageInfo x1 x2 x3 x4 x5 x6 x7 x8 x9 x10
                                  rnf r `seq` return r

instance Sizeable PackageInfo where
    dataOf _x                   = 10 .*. dataOfPtr
    statsOf x@(PackageInfo x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)
                                = mkStats x <> statsOf x1 <> statsOf x2 <> statsOf x3
                                            <> statsOf x4 <> statsOf x5 <> statsOf x6
                                            <> statsOf x7 <> statsOf x8 <> statsOf x9
                                            <> statsOf x10


-- ------------------------------------------------------------
