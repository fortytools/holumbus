{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- ----------------------------------------------------------------------------

module Hayoo.Search.XmlHtml
    ( RenderState (..)
    , result
    , renderXmlHtml
    )
where
import           Control.Monad
import           Control.Monad.Writer


import qualified Data.ByteString.Char8  as C
import           Data.Char              ( toLower
                                        , isSpace
                                        )
import           Data.Function
import qualified Data.List              as L
import qualified Data.Map               as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text              (Text, pack)

import           Holumbus.Utility hiding (escape, join)
import           Holumbus.Index.Common

import           Holumbus.Query.Result

import           Hayoo.IndexTypes
import           Hayoo.Search.Common

import           Text.XmlHtml

-- ------------------------------------------------------------
-- with these helper the XHtmlCombinators are simulated
-- with XmlHtml types.
-- XmlHtml defines the data types used with snap

type FlowContent = ()
type Table1Content = ()

data WS
    = Empty
    | Full ([Node] -> [Node])

fromWS :: WS -> [Node]
fromWS Empty
    = []
fromWS (Full f)
    = f []

instance Monoid WS where
    mempty = Empty

    mappend Empty     v2        = v2
    mappend v1        Empty     = v1
    mappend (Full v1) (Full v2) = Full $ v2 . v1

data XHtml a
    = X { _val :: a
        , _wrt :: WS
        }

instance Functor XHtml where
    f `fmap` m = m >>= return . f

instance Monad XHtml where
    return v = X v Empty
    (X v w) >>= f = let (X v' w') = f v in X v' (w `mappend` w')

instance MonadWriter WS XHtml where
    tell w = X () w
    listen (X v w) = X (v, w) w
    pass (X (v, f) w) = X v (f w)


type Attr  = (Text, Text)
type Attrs = [Attr]

tellNode :: Node -> XHtml ()
tellNode = tellNodes . (:[])

tellNodes :: [Node] -> XHtml ()
tellNodes ns = X () (Full $ \ xs -> xs ++ ns)

empty :: XHtml ()
empty = return ()

attr' :: Text -> Text -> Attr
attr' = (,)

text' :: Text -> XHtml ()
text' = tellNode . TextNode

text :: String -> XHtml ()
text = text' . pack

show' :: Show a => a -> XHtml ()
show' = text . show

elem' :: Text -> Attrs -> XHtml () -> XHtml ()
elem' name as body
    = tellNode $ Element name as (fromWS . _wrt $ body)

elem0 :: Text -> XHtml () -> XHtml ()
elem0 name
    = elem' name []

a_class_, a_href, a_id_, a_title, a_colspan, onclick :: Text -> Attr
a_class_  = attr' "class"
a_href    = attr' "href"
a_id_     = attr' "id"
a_title   = attr' "title"
a_colspan = attr' "colspan"
onclick   = attr' "onclick"

a', div', span', td', tr' :: Attrs -> XHtml () -> XHtml ()
a'        = elem' "a"
div'      = elem' "div"
span'     = elem' "span"
td'       = elem' "td"
tr'       = elem' "tr"

div_, table, tbody :: XHtml () -> XHtml ()
div_      = elem0 "div"
table     = elem0 "table"
tbody     = elem0 "tbody"

-- ------------------------------------------------------------

xhtml :: String -> XHtml ()
xhtml s
    | L.null s
        = ndg
    | otherwise
        = case parseXML "" . C.pack $ s of
            Left _  -> ndg
            Right d -> tellNodes . docContent $ d
    where
      ndg = text "No description."

-- ------------------------------------------------------------

data RenderState = RenderState
                 { rsQuery    :: String
                 , rsStart    :: Int
                 , rsStatic   :: Bool
                 }

pageLimit :: Int
pageLimit = 10

maxWordHits :: Int
maxWordHits = 75

staticRoot :: String
staticRoot = "hayoo.html"

renderXmlHtml :: XHtml () -> [Node]
renderXmlHtml
    = fromWS . _wrt

-- | Render the whole Hayoo! search result.
result :: RenderState -> StatusResult -> XHtml FlowContent
result rs (sm, rf, rp, tm, tp)
    = div' [a_id_ "result"] $
      do div' [a_id_ "status"] $ text sm  
         packageResults rs rp
         topLists rs tm tp
         functionResults rs rf
         div' [a_class_ "clear"] $ empty
         pager rs $ max (sizeDocHits rf) (sizeDocHits rp)

-- | The aggregation lists.
topLists :: RenderState -> [(String, Int)] -> [(String, Int)] -> XHtml FlowContent
topLists _ [] []
    = empty
topLists rs tm tp
    = div' [a_id_ "aggregation"] $
      do moduleList rs tm
         packageList rs tp

-- | The aggregated module list.
moduleList :: RenderState -> [(String, Int)] -> XHtml FlowContent
moduleList _ []
    = empty
moduleList rs tm
    = div' [a_id_ "modules"] $
      do div' [a_class_ "headline"] $ text "Top 15 Modules"
         mapM_ topModule (take 15 tm)
    where
      topModule (m, c)
          = div' [a_class_ "rootModule"] $
            do a' [ a_class_ "rootModuleName"
                  , a_href staticLink
                  , onclick dynamicLink
                  ] $ text m
               text " "
               span' [a_class_ "rootModuleCount"] $ show' c
          where
            staticLink
                = pack $
                  staticRoot ++ "?query=" ++ (rsQuery rs) ++ "%20module%3A" ++ m
            dynamicLink
                = pack $
                  "addToQuery('module:" ++ m ++ "'); return false;"

-- | The aggregated package list.
packageList :: RenderState -> [(String, Int)] -> XHtml FlowContent
packageList _ []
    = empty
packageList rs tp
    = div' [a_id_ "packages"] $
      do div' [a_class_ "headline"] $ text "Top 15 Packages"
         mapM_ topPackage (take 15 tp)
    where
      topPackage (k, c)
          = div' [a_class_ "package"] $
            do a' [ a_class_ "packageLink"
                  , a_href staticLink
                  , onclick dynamicLink
                  ] $ text k
               text " "
               span' [a_class_ "packageCount"] $ show' c
          where
            staticLink
                = pack $
                  staticRoot ++ "?query=" ++ (rsQuery rs) ++ "%20package%3A" ++ k
            dynamicLink
                = pack $
                  "addToQuery('package:" ++ k ++ "'); return false;"

-- | The Hackage hits (i.e. packages)
packageResults :: RenderState -> (Result PackageInfo) -> XHtml FlowContent
packageResults rs rp
    = let pl = extractPackages rp
      in 
        if L.null pl
        then empty
        else div' [a_id_ "hackage"] $
             mapM_ packageInfo pl
    where
      packageInfo k
          = div' [a_class_ "package"] $
            do div' [a_class_ "category"] $ mapM_ packageCategory (split "," $ p_category k)
               div' [a_class_ "name"] $
                    do a' [ a_class_ "name"
                          , a_href $ pack $ "http://hackage.haskell.org/package/" ++ (p_name k)
                          ] $ text (p_name k)
               span' [a_class_ "synopsis"] $ text (" " ++ (p_synopsis k))
               div' [a_class_ "description"] $ text (p_description k)
               div' [a_class_ "author"] $ text (p_author k)
      packageCategory c
          = a' [ a_class_ "category"
               , a_href $ pack $
                 "http://hackage.haskell.org/packages/archive/pkg-list.html#cat:" ++ map toLower c
               ] $ text c
      extractPackages
          = take pageLimit .
            drop (rsStart rs) . 
            (mapMaybe (\ (_, (di, _)) -> custom $ document di)) . 
            reverse .
            L.sortBy (compare `on` (docScore . fst . snd)) .
            toListDocIdMap .
            docHits

-- | The function hits.

functionResults :: RenderState -> (Result FunctionInfo) -> XHtml FlowContent
functionResults rs rf
    = do div' [a_id_ "words"] $
              mapM_ (wordInfo rs $ maxScoreWordHits rf) (toListSortedWords $ wordHits rf)
         div' [a_id_ "documents"] $
              table $ mapM_ functionInfo (toListSortedDocs $ docHits rf)
    where
      notSignature (_, (_, wch))
          = M.keys wch /= ["signature"]
      toListSortedDocs
          = take pageLimit .
            drop (rsStart rs) .
            reverse . 
            L.sortBy (compare `on` (docScore . fst . snd)) .
            toListDocIdMap
      toListSortedWords
          = L.sortBy (compare `on` fst) .
            take maxWordHits . 
            L.sortBy (compare `on` (wordScore .fst . snd)) .
            filter notSignature .
            M.toList

-- | Render the information about a function (module w/ link, function name, signature, ...)

functionInfo :: (DocId, (DocInfo FunctionInfo, DocContextHits)) -> XHtml Table1Content
functionInfo (_, (DocInfo (Document _ _ Nothing) _, _))
    = error "Expecting custom document info"
functionInfo (_, (DocInfo (Document t u (Just fi)) r, _))
    = tbody $
      do tr' [a_class_ "function"] $
             do td' [a_class_ "module"] $
                    a' [a_class_ "module", a_href $ pack (modLink u)] $ text (moduleName fi ++ ".")
                td' [a_class_ "name"] $
                    a' [a_class_ "function", a_href $ pack u, a_title $ pack ("Score: " ++ show r)] $ text t
                td' [a_class_ "signature"] $
                    sigDecl (signature fi)
         tr' [a_class_ "details"] $
             do td' [a_class_ "package"] $
                    a' [a_class_ "package", a_href $ pack (pkgLink $ package fi)] $ text (package fi)
                td' [a_class_ "description", a_colspan "2"] $
                    div_ $ do a' [a_class_ "toggleFold", onclick "toggleFold(this);"] $ empty
                              div' [a_class_ "description"] $ xhtml $ fctDescr fi
                              let c = sourceURI fi
                              if L.null c
                                 then empty
                                 else span' [a_class_ "source"] $
                                      a' [a_class_ "source", a_href $ pack c] $
                                      text "Source"
    where
      modLink
          = takeWhile ((/=) '#')
      pkgLink
          = (++) "http://hackage.haskell.org/package/"
      sigDecl s'
          | s' `elem` ["data", "type", "newtype", "class", "module"]
              = span' [a_class_ "declaration"] $ text s
          | otherwise
              = text $ replace "->" " -> " s
          where
            s = ":: " ++ stringTrim s'

-- | Render a word in the cloud of suggestions
wordInfo :: RenderState -> Score -> (Word, (WordInfo, WordContextHits)) -> XHtml FlowContent
wordInfo rs m (w, (WordInfo ts s, c)) = do
        a' [a_class_ "cloud", a_href staticLink, onclick dynamicLink, a_title $ pack origin] $ do
                span' [a_class_ $ pack $ "cloud" ++ (show ((round $ weightScore 1 9 m s)::Int))] $ text w
        text " "
        where
        origin = L.intercalate ", " $ M.keys c
        dynamicLink = pack $ "replaceInfQuery('" ++ escape t ++ "','" ++ escape w ++ "'); return false;"
        staticLink = pack $  staticRoot ++ "?query=" ++ (replace t w qu) ++ "&start=" ++ (show st)
        weightScore mi ma to v  = ma - ((to - v) / to) * (ma - mi)
        escape []      = []
        escape (x:xs)  = if x == '\'' then "\\'" ++ escape xs else x : (escape xs)
        t = head ts
        qu = rsQuery rs
        st = rsStart rs

pager :: RenderState -> Int -> XHtml FlowContent
pager rs m = if m <= 0 || m < pageLimit then empty else div'[a_id_ "pager"] $ do
                nav "previous" "<" (_prevPage pg)
                mapM_ page (_predPages pg)
                span' [a_class_ "current"] $ show' $  _currPage pg
                mapM_ page (_succPages pg)
                nav "next" ">" (_nextPage pg)
        where
        pg = makePager (rsStart rs) pageLimit m
        nav :: Text -> String -> Maybe Int -> XHtml FlowContent
        nav _ _ Nothing = empty
        nav c t (Just v) = a' [a_class_ c, a_href $ statLink v, onclick $ dynLink v] $ text t
        page (v, t)  = a' [a_class_ "page", a_href $ statLink v, onclick $ dynLink v] $ show' t
        dynLink v = pack $ "showPage(" ++ show v ++ "); return false;"
        statLink v = pack $ staticRoot ++ "?query=" ++ (rsQuery rs) ++ "&start=" ++ show v

-- ------------------------------------------------------------

data Pager  = Pager 
              { _prevPage  :: Maybe Int -- == last predPages
              , _predPages :: [(Int, Int)]
              , _currPage  :: Int
              , _succPages :: [(Int, Int)]
              , _nextPage  :: Maybe Int -- == head succPages
              }

-- Start element (counting from zero), elements per page, total number of elements.
makePager :: Int -> Int -> Int -> Pager
makePager s pg n = Pager pv (drop (length pd - 10) pd) (length pd + 1) (take 10 sc) nt
  where
  pv = if s < pg then Nothing else Just (s - pg)
  nt = if s + pg >= n then Nothing else Just (s + pg)
  pd = map (\x -> (x, x `div` pg + 1)) $ genPred s []
    where
    genPred rp tp = let np = rp - pg in
                                  if np < 0
                                  then tp
                                  else genPred np (np:tp)
  sc = map (\x -> (x, x `div` pg + 1)) $ genSucc s []
    where
    genSucc rs ts = let ns = rs + pg in
                                  if ns >= n
                                  then ts
                                  else genSucc ns (ts ++ [ns])

-- | Replace a given list with another list in a list.
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new l = L.intercalate new . split old $ l

-- | Remove leading and trailing whitespace using isSpace.
stringTrim :: String -> String
stringTrim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- ------------------------------------------------------------
