module Examples2.MandelWithoutReduce.ImageTypes
where 

import Data.Array.Unboxed
import Data.Word

-- ----------------------------------------

type Lightness	= Double

type Channel	= Int -> Int -> Lightness

data Geo
    = Geo { width  :: ! Int
	  , height :: ! Int
	  }
      deriving (Eq)

data Image
    = Image { geo :: ! Geo
	    , img :: Channel
	    }

-- ----------------------------------------

-- mothers little helper

on2		:: (a -> b) ->
		   (c -> d -> a) -> (c -> d -> b)
f `on2` c   	= \ x y -> f (c x y)


-- ----------------------------------------


gammaLight	:: Lightness -> Lightness -> Lightness
gammaLight g x	= x ** (1/g)


-- ----------------------------------------
--
-- predefined geometry values anf functions

instance Show Geo where
    show (Geo w h) = show w ++ "x" ++ show h

mkGeo	:: Int -> Int -> Geo
mkGeo w h
    | w > 0 && h > 0	= Geo w h
    | otherwise		= error $ "illegal geometry " ++ show (Geo w h)
-- ----------------------------------------

showImage	:: Bool -> Image -> String
showImage bin (Image (Geo w h) at)
    = imgType ++ imgGeo ++ "255\n" ++ imgData
    where
    imgGeo	= show w ++ " " ++ show h ++ "\n# Haskell PPM Tools\n"

    imgType
	| bin		= "P5\n"
	| otherwise	= "P2\n"

    imgData
	= pixToChar . pixToList $ at

    pixToChar
	| bin		= map toEnum
	| otherwise	= concatMap ((++ "\n") . show)

    pixToList f
	= [ toPix (f x y) | y <- [0..h-1], x <- [0..w-1] ]
	where
	toPix	:: Lightness -> Int
	toPix c = floor (c * 256.0) `min` 255 `max` 0

-- ----------------------------------------

gamma		:: Lightness -> Image -> Image
gamma g		= transLight (gammaLight g)

transLight :: (Lightness -> Lightness) ->
	      Image -> Image
transLight tf (Image g c)
		= Image g $ tf `on2` c


-- ----------------------------------------
-- save the image to disk
saveImage :: Geo -> [Lightness] -> FilePath -> IO ()
saveImage g pix dst = do
  putStrLn ("write wile: " ++ dst)
  let res = makeImage True g pix
  writeFile dst res
      

-- make the image in pgm format      
makeImage :: Bool -> Geo -> [Lightness] -> String
makeImage bin (Geo w h) ls = imgType ++ imgGeo ++ "255\n" ++ imgData
  where
  imgGeo  = show w ++ " " ++ show h ++ "\n# Haskell PPM Tools\n"

  imgType
    | bin   = "P5\n"
    | otherwise = "P2\n"

  imgData = pixToChar . pixToList $ ls

  pixToChar
    | bin   = map toEnum
    | otherwise = concatMap ((++ "\n") . show)

  pixToList ls = map toPix ls
    where
    toPix :: Lightness -> Int
    toPix c = floor (c * 256.0) `min` 255 `max` 0   
