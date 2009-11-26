{-# OPTIONS 
    -fno-warn-missing-signatures 
    -fno-warn-unused-matches 
    -fno-warn-type-defaults
#-}

module Main
where

import qualified Holumbus.Data.PrefixTree	as PT
import qualified Holumbus.Data.PrefixTreeCore	as PTC
import qualified Holumbus.Data.StrMap		as SM

import           Data.List
import           System.IO

main		:: IO ()
main		=
                do
		ws <- bibleWords
                putStr $ formatStats "prefix tree"              . ptStats $               mkPT ws
                putStr $ formatStats "prefix tree (normalized)" . ptStats $ PTC.deepNorm (mkPT ws)
                putStr $ formatStats "string map (trie)"        . smStats $               mkSM ws
    		return ()

-- ----------------------------------------

bibleWords	= do
                  hPutStrLn stderr "reading 'bibel.txt'"
                  c  <- readFile "bibel.txt"
                  ws <- return . words $ c
                  hPutStrLn stderr (("list of none ASCII words " ++) . show . filter (any (> '\127')) $ ws)
                  hPutStrLn stderr (show (length ws) ++ " words read")
                  return ws

-- ----------------------------------------

formatStats imp (s, k, l, v)
		= unlines $
                  [ ""
                  , "Space statistics for tree implementation '" ++ imp ++"'"
                  , "s: # of cell in tree       : " ++ ifmt 10 s
                  , "k: # of key chars          : " ++ ifmt 10 k
                  , "v: # of values             : " ++ ifmt 10 v
                  , "d: # of data cells         : " ++ ifmt 10 (k + v)                 ++ "  (k + v)"
                  , "l: sum of length s of keys : " ++ ifmt 10 l
                  , "sharing of keys (l/k)      : " ++ rfmt 10 2 k l                   ++ "  (l / k)"
                  , "space tree / space data    : " ++ rfmt 10 2 (s - (k + v)) (k + v) ++ "  (s - d) / d"
                  , ""
                  ]

ifmt d i	= reverse . take d  . (++ (replicate d  ' ')) . reverse . show $ i

rfmt d d2 i j	= reverse . take d . (++ (replicate d ' ')) . reverse . show $ r
        where
        sc              = 10 ^ d2
        i'		= i * sc + j `div` 2
        r               :: Double
        r               = fromIntegral ( i' `div` j) / fromIntegral sc

-- ----------------------------------------

mkPT            =  foldl' (\ t w -> PT.insertWith (+) w 1 t) PT.empty

ptStats ts	= (space, keyChars, keyLengths, values)
    where
    space	= PT.space ts
    keyChars	= PT.keyChars ts
    keyLengths  = sum . map length . PT.keys $ ts
    values      = PT.size ts

-- ----------------------------------------

mkSM            =  foldl' (\ t w -> SM.insertWith (+) w 1 t) SM.empty

smStats ts	= (space, keyChars, keyLengths, values)
    where
    space	= SM.space ts
    keyChars	= SM.keyChars ts
    keyLengths  = sum . map length . SM.keys $ ts
    values      = SM.size ts

-- ----------------------------------------
