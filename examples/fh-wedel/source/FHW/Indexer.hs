module Main
where
import           Control.Monad
import           Text.XML.HXT.Arrow
import           Holumbus.Build.Config
import           Holumbus.Build.Crawl
import           Holumbus.Build.Index
import           Holumbus.Index.Common
import           Holumbus.Index.Documents (Documents)
import qualified Holumbus.Index.Documents as DOC
import           Holumbus.Index.Inverted.Memory(emptyInverted)
import           Holumbus.Index.Cache
import           Holumbus.Utility

import qualified Data.IntMap as IM
import qualified Data.Set    as S
import Data.Maybe
import Data.Binary
import Data.List

main :: IO ()
main 
  = do
    let traceLevel     = 1
        workerThreads  = 1 
        docsPerCrawl   = 5
        docsPerIndex   = 250
        idxConfig      = ic_fhw
        crawlState     = initialCrawlerState idxConfig customFunction
    
    

{-    crawlState <- (loadCrawlerState "/home/sms/indexes/CS-FHW.bin" crawlState)
                  
    crawlState <- return $ crawlState
                  { cs_toBeProcessed = S.filter (cs_fCrawlFilter crawlState) (cs_toBeProcessed crawlState)
                  , cs_docs          = filterDocuments (\d -> (cs_fCrawlFilter crawlState) (uri d)) (cs_docs crawlState)
                  }
-}                  
                      
    {- runX (traceMsg 0 (" loading documents ... " ))
    theDocs <- loadFromBinFile ( (ic_idxPath idxConfig) ++ "-docs.bin") :: IO (Documents Int)
    crawlState     <- return $ fromDocuments crawlState theDocs
    -}
    
    -- ---------------------------------------------------------------------------------------------
    -- CRAWLING
    -- ---------------------------------------------------------------------------------------------
    runX (traceMsg 0 (" crawling  ----------------------------- " ))
    docs       <- crawl traceLevel workerThreads docsPerCrawl crawlState
--    docs <- loadFromBinFile "/home/sms/indexes/fhw-docs.bin" :: IO (Documents (Maybe Int))

    writeToXmlFile ( (ic_idxPath idxConfig) ++ "-docs.xml") (docs)
--    writeToBinFile ( (ic_idxPath idxConfig) ++ "-docs.bin") (docs)
        
    
    -- ---------------------------------------------------------------------------------------------
    -- INDEXING
    -- ---------------------------------------------------------------------------------------------
    runX (traceMsg 0 (" indexing  ----------------------------- " ))

    localDocs <- return $ tmpDocs (fromMaybe "/tmp" (ic_tmpPath idxConfig)) docs

    c <- createCache ((ic_idxPath idxConfig) ++ "-cache.db")

--    idx <- buildIndex workerThreads traceLevel localDocs idxConfig emptyInverted (Just c)
    pathes    <- buildSplitIndex 
                         workerThreads
                         traceLevel
                         localDocs
                         idxConfig
                         emptyInverted
                         True
                         docsPerIndex
                         
    idx       <- foldM mergeIndexes' emptyInverted pathes

    
    writeToXmlFile ( (ic_idxPath idxConfig) ++ "-index.xml") idx
    writeToBinFile ( (ic_idxPath idxConfig) ++ "-index.bin") idx    
  
    return()
    where
      mergeIndexes' i1 f = do
                           i2 <- loadFromBinFile (f  ++ "-index.bin")
                           return $ mergeIndexes i1 i2
                           
fromDocuments :: Binary a => CrawlerState a -> Documents a -> CrawlerState a
fromDocuments cs ds = cs { cs_toBeProcessed = S.fromList ( map (uri . snd) ( IM.toList $ DOC.toMap ds )) }
     
customFunction :: ArrowXml a => a XmlTree (Maybe Int)
customFunction = constA Nothing    

ic_test :: IndexerConfig
ic_test = ic_fhw {ic_startPages = ["http://trac.schlatt.com/test/index.html"], ic_fCrawlFilter = const True}

ic_fhw :: IndexerConfig
ic_fhw 
  = IndexerConfig
    { ic_startPages     = [ "http://www.fh-wedel.de/"
                          , "http://www.fh-wedel.de/sonstiges/sitemap/"
                          , "http://www.fh-wedel.de/wir-ueber-uns/mitarbeiter-innen/?no_cache=1"
                          ]
    , ic_tmpPath        = Just "/tmp/"
    , ic_idxPath        = "/home/sms/indexes/fhw"
    , ic_contextConfigs = ccs_fhw
    , ic_readAttributes = standardReadDocumentAttributes
    , ic_fCrawlFilter   = simpleCrawlFilter -- [ "^http://www\\.fh-wedel\\.de"] -- 
                                            ["^http://[a-z]*\\.?fh-wedel\\.de" ]           -- allow
                                        (["tx_fhwunternehmensforum_pi3"                     -- deny
                                        , "http://asta.fh-wedel.de"                -- slow
                                        , "http://biblserv.fh-wedel.de"            -- slow
                                        , "http://darcs.fh-wedel.de"               -- hackers only
                                        , "http://stud.fh-wedel.de"                -- boring
                                        , "http://holumbus.fh-wedel.de/branches"
                                        , "http://holumbus.fh-wedel.de/cgi-bin"
                                        , "/HXmlToolbox/hdoc", "si/doc/javadoc/docs"
                                        , "~herbert/html", "/java/jdk1.1.1/docs"
--                                       , "/~", "/%7E", "http://www.fh-wedel.de/mitarbeiter/"
                                        , "\\?L=0", "\\&L=0"
                                        , ".pdf$", ".jpg$", ".gif$", ".png$", ".tar.gz$"
                                        , ".ppt$", ".exe$", ".txt$", ".zip$", ".doc$"
                                        , ".dot$", ".png$", ".ps$", ".ps.gz$", ".nb$"
                                        , ".swf$", ".JPG$", ".tex$", ".rss$", ".mpg$"
                                        , ".mp3$", ".m3u$", ".java$", ".tgz$", ".svg", ".mdb$" 
                                        , ".PDF$", ".xls$", ".dta$", ".lst$", ".rar", ".avi$"
                                        , "%7Edi", "/~di"
                                        , "ws99/Ausarbeitung/mico/Beispiel"
                                        , "/rundgang/id=", "/vorlesungsplan/id="
                                        , "/vorlesungsplan/sem=", "/tv-infosystem/", "/~splan/"
                                        , "http://www\\.fh-wedel\\.de/index\\.php\\?eID=tx_cms_showpic"
                                        , "http://www.fh-wedel.de/fileadmin/mitarbeiter/ne/CG/opengl_man"
                                        , "http://www.fh-wedel.de/%7Esi/vorlesungen/c/beispiele"
                                        , "http://www.fh-wedel.de/~si/vorlesungen/c/beispiele"
                                        , "http://www.fh-wedel.de/~wol/fhtml" -- very slow and boring pages
                                        , "http://www.fh-wedel.de/%7Esi/vorlesungen/internet/TclTk/program1.html" -- slow
                                        ] ++ list404)
    }
    
    
    
list404 :: [String]
list404 = 
  [ "http://www.fh-wedel.de/work/medienlabor-der-fh-wedel/raeumlichkeiten/rechenzentrum/"
  , "http://www.fh-wedel.de/typo3"
  , "http://www.fh-wedel.de/~ue"
  , "http://www.fh-wedel.de/~wk"
  , "http://www.fh-wedel.de/work/medienlabor-der-fh-wedel/raeumlichkeiten/videostudio/"
  , "http://www.fh-wedel.de/studierende/campus-infos/fh-stream/team.php"
  , "http://www.fh-wedel.de/wir-ueber-uns/labore-fe/informatik/research/index.html"
  , "http://www.fh-wedel.de/studierende/campus-infos/fh-stream/streams.php"
  , "http://www.fh-wedel.de/wir-ueber-uns/labore-fe/informatik/vrlab/index.html"
  , "http://www.fh-wedel.de/wir-ueber-uns/labore-fe/informatik/refs/index.html"
  , "http://www.fh-wedel.de/studierende/campus-infos/fh-stream/specials.php"
  , "http://www.fh-wedel.de/wir-ueber-uns/labore-fe/informatik/virtual-reality-labor/index.html"
  , "http://www.fh-wedel.de/wir-ueber-uns/labore-fe/informatik/index.html"
  , "http://www.fh-wedel.de/studierende/campus-infos/fh-stream/kontakt.php"
  , "http://www.fh-wedel.de/wir-ueber-uns/labore-fe/informatik/virtual-reality-labor/cobench/index.html"
  , "http://www.fh-wedel.de/studierende/campus-infos/online-tutorials/vlc-usage.htm"
  , "http://www.fh-wedel.de/studierende/campus-infos/fh-stream/index.php"
  , "http://www.fh-wedel.de/wir-ueber-uns/labore-fe/informatik/courses/index.html"
  , "http://www.fh-wedel.de/wir-ueber-uns/labore-fe/informatik/virtual-reality-labor/cave/index.html"
  , "http://www.fh-wedel.de/studierende/campus-infos/online-tutorials/toc_index.htm"
  , "http://www.fh-wedel.de/wir-ueber-uns/labore-fe/informatik/vrlab/cobench/index.html"
  , "http://www.fh-wedel.de/wir-ueber-uns/labore-fe/informatik/news/index.html"
  , "http://www.fh-wedel.de/wir-ueber-uns/labore-fe/informatik/contact/index.html"
  , "http://www.fh-wedel.de/studierende/campus-infos/fh-stream/faq.php"
  , "http://www.fh-wedel.de/internationaloffice/Deutsch/"
  , "http://www.fh-wedel.de/%7Ege/Seminar.htm"
  , "http://www.fh-wedel.de/HXmlToolbox/index.html"
  , "http://www.fh-wedel.de/Literatur_08.html"
  , "http://www.fh-wedel.de/Seminar_Algorithmische_Graphentheorie.html"
  , "http://www.fh-wedel.de/Seminar_Kombinatorik.html"
  , "http://www.fh-wedel.de/absolventen/members/absolventensuche.html"
  , "http://www.fh-wedel.de/analysis-im-Netz.htm"
  , "http://www.fh-wedel.de/bba/index.html"
  , "http://www.fh-wedel.de/buecher.html"
  , "http://www.fh-wedel.de/buero_n15.html"
  , "http://www.fh-wedel.de/c/index.html"
  , "http://www.fh-wedel.de/cg1/index.html"
  , "http://www.fh-wedel.de/cg2/index.html"
  , "http://www.fh-wedel.de/cprog.html"
  , "http://www.fh-wedel.de/ctrl.html"
  , "http://www.fh-wedel.de/dbprog.html"
  , "http://www.fh-wedel.de/diplom.html"
  , "http://www.fh-wedel.de/download.htm"
  , "http://www.fh-wedel.de/downloads.html"
  , "http://www.fh-wedel.de/dozent.html"
  , "http://www.fh-wedel.de/faelle.html"
  , "http://www.fh-wedel.de/gesetze.html"
  , "http://www.fh-wedel.de/gisela.html"
  , "http://www.fh-wedel.de/handout.html"
  , "http://www.fh-wedel.de/index.html"
  , "http://www.fh-wedel.de/index/index.html"
  , "http://www.fh-wedel.de/infos/mitarbeiter"
  , "http://www.fh-wedel.de/infos/mitarbeiter/index.html"
  , "http://www.fh-wedel.de/infos/sprechzeiten/"
  , "http://www.fh-wedel.de/klausuren.html"
  , "http://www.fh-wedel.de/~si/seminare/ws05/Ausarbeitung/7.concurrent/index.html"
  , "http://www.fh-wedel.de/cis/semesterinfo/richtlinien/doku-richtl.html"
  , "http://www.fh-wedel.de/~si/seminare/ws06/Ausarbeitung/04.UserModeLinux/index.html"
  , "http://www.fh-wedel.de/cis/semesterinfo/studienordnung/index.html"
  , "http://www.fh-wedel.de/~ge/Datenbanken.htm"
  , "http://www.fh-wedel.de/~si/seminare/ws06/Ausarbeitung/14.Cocoon/cocoon0.htm"
  , "http://www.fh-wedel.de/~si/seminare/ws00/Ausarbeitung/index.html"
  , "http://www.fh-wedel.de/~si/seminare/ws01/Ausarbeitung/index.html"
  , "http://www.fh-wedel.de/%7Efi/odb/index.html"
  , "http://www.fh-wedel.de/~si/seminare/ws02/Ausarbeitung/index.html"
  , "http://www.fh-wedel.de/%7Ekap/"
  , "http://www.fh-wedel.de/~si/seminare/ws99/Ausarbeitung/index.html"
  , "http://www.fh-wedel.de/%7Eps2/programmierstil/delphi_progstil.html"
  , "http://www.fh-wedel.de/%7Eps2/programmierstil/inline_doku.html"
  , "http://www.fh-wedel.de/%7Eps2/programmierstil/pascal_progstil.html"
  , "http://www.fh-wedel.de/~mo/lectures/seminar-sose03.html"
  , "http://www.fh-wedel.de/~si/seminare/ws03/Ausarbeitung/index.html"
  , "http://www.fh-wedel.de/~si/HXmlToolbox/hdoc/index.htm"
  , "http://www.fh-wedel.de/archiv/iw/Lehrveranstaltungen/WS2006/GdT.html"
  , "http://www.fh-wedel.de/~si/seminare/ws04/Ausarbeitung/index.html"
  , "http://www.fh-wedel.de/~si/seminare/ws04/Termine/www.wikipedia.org"
  , "http://www.fh-wedel.de/~ki/ia/sem3/seminar1/code.htm"
  , "http://www.fh-wedel.de/klausuren/Compilerbau.html"
  , "http://www.fh-wedel.de/klausuren/CundOOP.html"
  , "http://www.fh-wedel.de/klausuren/SD.html"
  , "http://www.fh-wedel.de/klausuren/Unix.html"
  , "http://www.fh-wedel.de/klausuren/index.html"
  , "http://www.fh-wedel.de/klr2.html"
  , "http://www.fh-wedel.de/kontakt.html"
  , "http://www.fh-wedel.de/lebenslauf.html"
  , "http://www.fh-wedel.de/links.htm"
  , "http://www.fh-wedel.de/links.html"
  , "http://www.fh-wedel.de/material.html"
  , "http://www.fh-wedel.de/mec_MInf.html"
  , "http://www.fh-wedel.de/mitarbeiter/iw/lehrveranstaltungen-im-naechsten-semester/informatik-seminar-algorithmen/"
  , "http://www.fh-wedel.de/oop/index.html"
  , "http://www.fh-wedel.de/or.html"
  , "http://www.fh-wedel.de/personal.html"
  , "http://www.fh-wedel.de/phys1_TInf.html"
  , "http://www.fh-wedel.de/phys1_WIng.html"
  , "http://www.fh-wedel.de/planspiel.html"
  , "http://www.fh-wedel.de/planspiel2.html"
  , "http://www.fh-wedel.de/pmod.html"
  , "http://www.fh-wedel.de/praktika/MultimediaProjekte/SVG/SVG_Tutorial_mi3794/index.htm"
  , "http://www.fh-wedel.de/praktika/MultimediaProjekte/index.html"
  , "http://www.fh-wedel.de/praktika/SoftwarePraktikum/index.html"
  , "http://www.fh-wedel.de/projekt.html"
  , "http://www.fh-wedel.de/projekte/aktuell.htm"
  , "http://www.fh-wedel.de/pse.html"
  , "http://www.fh-wedel.de/rechnernetze/nwslab.htm"
  , "http://www.fh-wedel.de/seite2.htm"
  , "http://www.fh-wedel.de/seminare.html"
  , "http://www.fh-wedel.de/seminare/index.html"
  , "http://www.fh-wedel.de/seminare/ss03/Termine/Themen.html"
  , "http://www.fh-wedel.de/seminare/ss04/Termine/Themen.html"
  , "http://www.fh-wedel.de/seminare/ws03/Termine/Themen.html"
  , "http://www.fh-wedel.de/seminare/ws04/Termine/Themen.html"
  , "http://www.fh-wedel.de/seminare/ws05/Termine/Themen.html"
  , "http://www.fh-wedel.de/seminare/ws06/Termine/Themen.html"
  , "http://www.fh-wedel.de/sprechstunde.htm"
  , "http://www.fh-wedel.de/suche/"
  , "http://www.fh-wedel.de/termine.htm"
  , "http://www.fh-wedel.de/termine/Klausureinsicht.html"
  , "http://www.fh-wedel.de/termine/Sprechstunde.html"
  , "http://www.fh-wedel.de/termine/index.html"
  , "http://www.fh-wedel.de/toc_index.htm"
  , "http://www.fh-wedel.de/uebungen/hinweise/hinweise.html"
  , "http://www.fh-wedel.de/uebungen/oop/aufgaben/aufg04.html"
  , "http://www.fh-wedel.de/uebungen/oop/aufgaben/aufg05.html"
  , "http://www.fh-wedel.de/uebungen/oop/index.html"
  , "http://www.fh-wedel.de/uebungen/oop/richtlinien.html"
  , "http://www.fh-wedel.de/uebungen/unix/index.html"
  , "http://www.fh-wedel.de/veranstaltungen.htm"
  , "http://www.fh-wedel.de/vl.html"
  , "http://www.fh-wedel.de/vlc-usage.htm"
  , "http://www.fh-wedel.de/vorlesungen/c/c.html"
  , "http://www.fh-wedel.de/vorlesungen/cb/cb.html"
  , "http://www.fh-wedel.de/vorlesungen/fp/fp.html"
  , "http://www.fh-wedel.de/vorlesungen/index.html"
  , "http://www.fh-wedel.de/vorlesungen/internet/Literatur/Buecher.html"
  , "http://www.fh-wedel.de/vorlesungen/internet/internet.html"
  , "http://www.fh-wedel.de/vorlesungen/java/Literatur/Buecher.html"
  , "http://www.fh-wedel.de/vorlesungen/java/java.html"
  , "http://www.fh-wedel.de/vorlesungen/softwaredesign/Literatur/Buecher.html"
  , "http://www.fh-wedel.de/vorlesungen/softwaredesign/design.html"
  , "http://www.fh-wedel.de/vr/index.html"
  , "http://www.fh-wedel.de/xml2html.2/xml2html/xml/index.xml"
  , "http://www.fh-wedel.de/zettelkasten/etc/archiv.html"
  , "http://www.fh-wedel.de/zettelkasten/etc/index.html"
  , "http://www.fh-wedel.de/zettelkasten/etc/schwob.html"
  , "http://www.fh-wedel.de/zettelkasten/fortune/fhspruch.html"
  , "http://www.fh-wedel.de/zettelkasten/fortune/index.html"
  , "http://www.fh-wedel.de/zettelkasten/fortune/index.html?KEY=1147779735"
  ]  
  
-- | The list of context configurations for the Fh Wedel pages
ccs_fhw :: [ContextConfig]
ccs_fhw = []
      ++ [cc_title]
      ++ [cc_meta]
      ++ [cc_content]
--      ++ [cc_raw]

    
-- | Context for title-tags
cc_title :: ContextConfig
cc_title
  = ContextConfig
    { cc_name         = "title"
    , cc_preFilter    = this
    , cc_XPath        = "/html/head/title"  
    , cc_fTokenize    = map (stripWith (=='.')) . (parseWords isWordChar)
    , cc_fIsStopWord  = const False -- (\s -> length s < 2)
    , cc_addToCache   = False
    }
    
-- | Context for meta information. Description and keywords will be indexed
cc_meta :: ContextConfig
cc_meta
  = ContextConfig
    { cc_name         = "meta"
    , cc_preFilter    = this
    , cc_XPath        = "/html/head/meta[@name='description' or @name='keywords']/@content"  
    , cc_fTokenize    = map (stripWith (=='.')) . (parseWords isWordChar)
    , cc_fIsStopWord  = (\s -> length s < 2)
    , cc_addToCache   = False
    }    
    
-- | Context for normal page content. This indexes everything that is inside a div element with id
--   "col2_content" as it is defined in the central fhw template. Pages that do not use the normal
--   template will only be indexed in the raw context
cc_content :: ContextConfig
cc_content
  = ContextConfig
    { cc_name         = "content"
    , cc_preFilter    = this
    , cc_XPath        = "//div[@id='col2_content']"  
    , cc_fTokenize    = map (stripWith (=='.')) . (parseWords isWordChar)
    , cc_fIsStopWord  = (\s -> length s < 2)
    , cc_addToCache   = True
    }

-- | Raw context. This indexes all the text that is listed on the page without check if it is in
--   a menu or if it is real text. This context will have very low importance for search results
cc_raw :: ContextConfig
cc_raw
  = ContextConfig
    { cc_name         = "raw"
    , cc_preFilter    = this
    , cc_XPath        = "//body"  
    , cc_fTokenize    = map (stripWith (=='.')) . (parseWords isWordChar)
    , cc_fIsStopWord  = (\s -> length s < 2)
    , cc_addToCache   = False
    }
    
        
