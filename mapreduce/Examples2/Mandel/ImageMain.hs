module Main where

import Examples2.Mandel.ImageTypes
import Examples2.Mandel.ImageMandel

import System.IO
import System.Environment

saveImage :: Geo -> Double -> Int -> FilePath -> IO ()
saveImage g zmax iter dst
    = do
      writeFile dst res
      where
        image = Image g (imageMandel g zmax iter)
        --image' = gamma 4.0 image
        res = showImage True image
-- --------------------------------------

getParams	:: IO [String]
getParams
    = do
      args <- getArgs
      return (args ++ repeat "")

main	:: IO()
main
    = do
      args <-getParams
      main2 args
      
main2	:: [String] -> IO()
main2 (sw : sh : szmax : siter : outp : _)
    = saveImage (Geo w h) zmax iter outp
  	where
  	w = read sw
  	h = read sh
  	iter = read siter
	zmax = read szmax

-- --------------------------------------

