module Examples2.Mandel.ImageTypes
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

merge		:: (a -> b -> c) ->
		   (x -> y -> a) ->
		   (x -> y -> b) ->
		   (x -> y -> c)

merge op f1 f2	= \ x y -> f1 x y `op` f2 x y

-- ----------------------------------------

-- predefined Lightness values

dark		:: Lightness
dark		= 0.0

light		:: Lightness
light		= 1.0

grey		:: Lightness
grey		= 0.5

lightOrDark	:: Lightness -> Lightness
lightOrDark c
    | c <= 0.5	= 0.0
    | otherwise = 1.0

gammaLight	:: Lightness -> Lightness -> Lightness
gammaLight g x	= x ** (1/g)

invertLight	:: Lightness -> Lightness
invertLight	= (1.0 -)

reduceLight	:: Int -> Lightness -> Lightness
reduceLight n c	= fromIntegral c' / fromIntegral (n - 1)
                where
		c' :: Int
		c' = floor (fromIntegral n * c)

-- ----------------------------------------

-- predefined channel values and combinators

uniChan		:: Lightness -> Channel
uniChan		= const . const

darkChan	:: Channel
darkChan	= uniChan dark

lightChan	:: Channel
lightChan	= uniChan light

mergeChan	:: Lightness -> Channel -> Channel -> Channel
mergeChan a c1 c2
    		= \ x y -> a * c1 x y + (1 - a) * c2 x y

addChan		:: Channel -> Channel -> Channel
addChan		= mergeChan grey

addChans	:: [Channel] -> Channel
addChans cs	= \ x y ->
		  sum (map (\ c -> c x y) cs)
		  / fromIntegral (length cs)

rectangleChan	:: Geo -> Channel
rectangleChan (Geo w h)
		= \ x y	->
		  if 0 <= x && x < w && 0 <= y && y < h
		  then light
		  else dark

-- ----------------------------------------
--
-- predefined geometry values anf functions

instance Show Geo where
    show (Geo w h) = show w ++ "x" ++ show h

mkGeo	:: Int -> Int -> Geo
mkGeo w h
    | w > 0 && h > 0	= Geo w h
    | otherwise		= error $ "illegal geometry " ++ show (Geo w h)

maxGeo					:: Geo -> Geo -> Geo
(Geo w1 h1) `maxGeo` (Geo w2 h2)	= Geo (w1 `max` w2) (h1 `max` h2)

minGeo					:: Geo -> Geo -> Geo
(Geo w1 h1) `minGeo` (Geo w2 h2)	= Geo (w1 `min` w2) (h1 `min` h2)

flipGeo			:: Geo -> Geo
flipGeo (Geo w h)	= Geo h w

scaleGeo		:: Int -> Int -> Geo -> Geo
scaleGeo n m (Geo w h)	= mkGeo (n * w) (m * h)

shiftGeo		:: Int -> Int -> Geo -> Geo
shiftGeo n m (Geo w h)	= mkGeo (w + n) (h + m)

-- ----------------------------------------

mkBitmap	:: Geo -> (Int -> Int -> Bool) -> Image
mkBitmap g f	= Image g ((fromIntegral . fromEnum) `on2` f)


-- ----------------------------------------

readImage	:: String -> Image
readImage	= readPNM . item

readPNM		:: (String, String) -> Image
readPNM ("P1", str)
    = mkBitmap (mkGeo w h) at
      where
      (ns, rest)= readRow 2 str
      [w, h]	= map read ns
      pixels	= fst . readRow len $ rest
      len	= w * h

      bits	:: [Int]
      bits	= map read pixels

      bitMx     :: UArray Int Bool
      bitMx     = listArray (0, len - 1) . map (== 0) $ bits

      at x y
	  | 0 <= x && x < w &&
	    0 <= y && y < h
		=  bitMx ! (x + w * y)
	  | otherwise
	      = False

readPNM ("P2", str)
    = Image (mkGeo w h) at
      where
      ([w', h', m'], rest)	= readRow 3 str
      w		= (read w')::Int
      h		= (read h')::Int
      m		= (read m')::Lightness
      l		= w * h
      pixels	= fst . readRow l $ rest

      bytes	:: [Word8]
      bytes	= map read pixels

      byteMx	:: UArray Int Word8
      byteMx    = listArray (0, l - 1) bytes

      at x y
	  | 0 <= x && x < w &&
	    0 <= y && y < h
		=  fromIntegral (byteMx ! (x + w * y)) / m
	  | otherwise
	      = dark

readPNM ("P3", str)
    = Image (mkGeo w h) at
      where
      ([w', h', m'], rest)	= readRow 3 str
      w		= (read w')::Int
      h		= (read h')::Int
      m		= (read m')::Lightness
      len	= w * h
      len3	= len * 3
      pixels	= fst . readRow len3 $ rest

      bytes	:: [Word8]
      bytes	= merge3 (map read pixels)
		  where
		  merge3 (r:g:b:xs) = (r + g + b) `div` 3 : merge3 xs
		  merge3 _          = []

      byteMx	:: UArray Int Word8
      byteMx    = listArray (0, len - 1) bytes

      at x y
	  | 0 <= x && x < w &&
	    0 <= y && y < h
		=  fromIntegral (byteMx ! (x + w * y)) / m
	  | otherwise
	      = dark

readPNM ("P5", str)
    = Image (mkGeo w h) at
      where
      ([w', h', m'], rest)	= readRow 3 str
      w		= (read w')::Int
      h		= (read h')::Int
      m		= (read m')::Lightness
      l		= w * h

      bytes	:: [Word8]
      bytes	= map (toEnum . fromEnum) .tail $ rest

      byteMx	:: UArray Int Word8
      byteMx    = listArray (0, l - 1) bytes

      at x y
	  | 0 <= x && x < w &&
	    0 <= y && y < h
		=  fromIntegral (byteMx ! (x + w * y)) / m
	  | otherwise
	      = dark

readPNM ("P6", str)
    = Image (mkGeo w h) at
      where
      ([w', h', m'], rest)	= readRow 3 str
      w		= (read w')::Int
      h		= (read h')::Int
      m		= (read m')::Lightness
      len	= w * h

      bytes	:: [Word8]
      bytes	= merge3 . map (toEnum . fromEnum) . tail $ rest
		  where
		  merge3 (r:g:b:xs) = (r + g + b) `div` 3 : merge3 xs
		  merge3 _          = []

      byteMx	:: UArray Int Word8
      byteMx    = listArray (0, len - 1) bytes

      at x y
	  | 0 <= x && x < w &&
	    0 <= y && y < h
		=  fromIntegral (byteMx ! (x + w * y)) / m
	  | otherwise
	      = dark


readPNM (fmt, _)
    = error $ "unsupported PNM format " ++ show fmt

-- ----------------------------------------

item	:: String -> (String, String)
item str
    = i1
      where
      i@(w,r) = head (lex str)
      i1 | head w == '#'
	     = item r1
	 | otherwise
	     = i
	 where
	 r1 = ( drop 1 . snd . break (== '\n') . drop 1) r

readFct	:: (String -> (a, String)) ->
	   Int ->
	   String -> ([a], String)

readFct _f 0 str
    = ([],str)

readFct f n str
    = (i:r, str2)
      where
      (i, str1)	= f str
      (r, str2)	= readFct f (n-1) str1

readRow		:: Int -> String -> ([String], String)
readRow	 	= readFct item


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

invert		:: Image -> Image
invert		= transLight invertLight

gamma		:: Lightness -> Image -> Image
gamma g		= transLight (gammaLight g)

bitmap		:: Image -> Image
bitmap		= transLight lightOrDark

reduceColors	:: Int -> Image -> Image
reduceColors n	= transLight (reduceLight n)

transLight :: (Lightness -> Lightness) ->
	      Image -> Image
transLight tf (Image g c)
		= Image g $ tf `on2` c

-- ----------------------------------------

flipV		:: Image -> Image
flipV (Image g@(Geo w _h) c)
		= Image g $ \ x y -> c (w - x - 1) y

flipH		:: Image -> Image
flipH (Image g@(Geo _w h) c)
		= Image g $ \ x y -> c x (h - y - 1)

rot90		:: Image -> Image
rot90 (Image (Geo w h) c)
		= Image (Geo h w) $ \ x y -> c y (h - x - 1)

rot180		:: Image -> Image
rot180		= flipV . flipH

rot270		:: Image -> Image
rot270		= rot90 . rot180

-- das Bild um n Spalten nach rechts rotieren und
-- um m Zeilen nach unten

shiftRot	:: Int -> Int -> Image -> Image
shiftRot n m (Image g@(Geo w h) c)
		= Image g $ \ x y -> c ((x - n) `mod` w) ((y - m) `mod` h)

-- das Bild n mal in der Wagerechten und
-- m mal in der Senkrechten wiederhohlen
-- (kacheln)

tile		:: Int -> Int -> Image -> Image
tile n m (Image (Geo w h) c)
		= Image (Geo (w*n) (h*m)) $ \ x y -> c (x `mod` w) (y `mod` h)

-- das gleiche wie tile, nur mit abwechselnd gespiegelten
-- Bildern, so dass keine sichtbaren Kanten an den Bildgrenzen
-- entstehen

tileMirr	:: Int -> Int -> Image -> Image
tileMirr n m (Image (Geo w h) c)
		= Image (Geo (w*n) (h*m)) $
 		  \ x y
		      -> let
			 fw = f1 w
			 fh = f1 h
			 in
			 c (fw x) (fh y)
                  where
		  f1 l z
		      | even zd	  = zm
		      | otherwise = l - zm - 1
		      where
		      zd = z `div` l
		      zm = z `mod` l


-- Groesse halbieren

halfSize	:: Image -> Image
halfSize (Image (Geo w h) c)
		= Image (Geo ((w + 1) `div` 2) ((h + 1) `div` 2)) $
		  \ x y -> let
			   x1 =  2 * x
			   x2 = (2 * x + 1) `min` (w - 1)
			   y1 =  2 * y
			   y2 = (2 * y + 1) `min` (h - 1)
			   in
			   ( c x1 y1 + c x1 y2 + c x2 y1 + c x2 y2) / 4

-- vergroessern um die Faktoren n un m

scale		:: Int -> Int -> Image -> Image
scale n m (Image (Geo w h) c)
		= Image (Geo (w*n) (h*m)) $
		  \ x y -> c (x `div` n) (y `div` m)

-- Zwei Bilder kombinieren, z.B. addieren

merge2		:: (Lightness -> Lightness -> Lightness) ->
		   Image -> Image -> Image
merge2 op (Image (Geo w1 h1) c1) (Image (Geo w2 h2) c2)
		= Image (Geo (w1 `min` w2) (h1 `min`h2)) $
		  \ x y -> c1 x y `op` c2 x y

-- ----------------------------------------

