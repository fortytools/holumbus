module Main
(
  main
)
where

import           Holumbus.Common.Logging
import           Holumbus.Common.Utils                          ( handleAll )

import           Holumbus.Network.PortRegistry.PortRegistryPort
import qualified Holumbus.Distribution.DMapReduce               as MR
import           Holumbus.MapReduce.Types
import           Holumbus.MapReduce.MapReduce 

import           Examples2.Mandel.DMandel
import           Examples2.Mandel.ImageTypes
import System.IO
import System.Environment
import Data.List
    
version :: String
version = "Distributed Mandel-set calculation"


-- save the image to disk
saveImage :: Geo -> [Lightness] -> FilePath -> IO ()
saveImage g pix dst
    = do
      let res = makeImage True g pix
      writeFile dst res
      

-- make the image in pgm format      
makeImage	:: Bool -> Geo -> [Lightness] -> String
makeImage bin (Geo w h) ls
    = imgType ++ imgGeo ++ "255\n" ++ imgData
    where
    imgGeo	= show w ++ " " ++ show h ++ "\n# Haskell PPM Tools\n"

    imgType
	| bin		= "P5\n"
	| otherwise	= "P2\n"

    imgData
	= pixToChar . pixToList $ ls

    pixToChar
	| bin		= map toEnum
	| otherwise	= concatMap ((++ "\n") . show)

    pixToList ls
	= map toPix ls
	where
	toPix	:: Lightness -> Int
	toPix c = floor (c * 256.0) `min` 255 `max` 0      

getParams	:: IO [String]
getParams
    = do
      args <- getArgs
      return (args ++ repeat "")
      
main	:: IO()
main
    = do
      args <-{-#SCC "getargscc" #-}getParams
      {-#SCC "getargscc" #-} main2 args

main2 :: [String] -> IO ()
main2 (sw : sh : szmax : siter : outp : _)
  = do
    putStrLn version
    handleAll (\e -> putStrLn $ "EXCEPTION: " ++ show e) $
      do
      initializeLogging []
      p <- newPortRegistryFromXmlFile "/tmp/registry.xml"
      setPortRegistry p      
      mr <- initializeData

      (ls,_) <- doMapReduce (dmandelAction) (w,h,zmax,iter) (pixels w h) [] 2 2 2 2 TOTRawTuple mr
--      let pix = (concat . map (\(xk,vs) -> vs) . sortBy sortPixels) ls      
      let pix = (concat . map (\(xk,vs) -> vs)) ls
      saveImage (Geo w h) pix outp
      deinitializeData mr
    where
  	w = read sw
  	h = read sh
  	iter = read siter
  	zmax = read szmax

sortPixels :: (Ord k) => (k,v) -> (k,v) -> Ordering
sortPixels (k1,_) (k2,_) 
  | k1 > k2 = GT
  | k1 < k2 = LT
  | otherwise = EQ
  

initializeData :: IO (MR.DMapReduce)
initializeData 
  = do    
    let config = MR.defaultMRClientConfig
    MR.mkMapReduceClient config


deinitializeData :: MR.DMapReduce -> IO ()
deinitializeData mr
  = do
    MR.closeMapReduce mr

pixels :: Int -> Int -> [(Int,[Int])]
pixels w h
  = [(y,take w [0..])|y<-[0..h-1]]   
-- ----------------------------------------------------------------------------
