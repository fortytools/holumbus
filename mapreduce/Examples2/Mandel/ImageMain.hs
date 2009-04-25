module Main where

import Examples2.Mandel.ImageTypes
import Examples2.Mandel.ImageMandel

import System.IO
import System.Environment

saveImage :: (RealFloat a) =>  Geo -> a -> Int -> FilePath -> IO ()
saveImage g zmax iter dst
    = do
      let res = showImage True $ gamma 4.0 $ Image g (imageMandel g zmax iter)
      writeFile dst res
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
    = do
--      print $ "Width : " ++ show w
--      print $ "Height: " ++ show h
--      print $ "Iter  : " ++ show iter
--      print $ "ZMax  : " ++ show zmax;
      saveImage (Geo w h) zmax iter outp
  	where
  	w = read sw
  	h = read sh
  	iter = read siter
  	zmax = read szmax

-- --------------------------------------

