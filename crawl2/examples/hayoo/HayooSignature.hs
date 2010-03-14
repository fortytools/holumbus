module HayooSignature
where
 
import		 Data.Char
import		 Data.List
import qualified Data.Map		as M

import           Holumbus.Utility

-- ------------------------------------------------------------

-- | Normalizes a Haskell signature, e.g. @String -> Int -> Int@ will be transformed to 
-- @a->b->b@. All whitespace will be removed from the resulting string.

normalizeSignature 		:: String -> String
normalizeSignature 		= join "->" . (replaceTypes M.empty ['a'..'z']) . split "->" . filter (not . isSpace)
  where
  replaceTypes _ _ [] 		= []
  replaceTypes v t (x:xs) 	= let
                                  (nv, ut, rx) = replace'
                                  in
                                  rx:(replaceTypes nv ut xs)
    where
    replace' 			= let
                                  ut = [head t]
                                  in
                                  maybe (M.insert r ut v, tail t, ut) (\n -> (v, t, n)) (M.lookup r v)
        where r 		= stripWith (\c -> (c == '(') || (c == ')')) x


getSignature 			:: String -> String
getSignature s 			= stripWith (==' ') $ 
                                  if "=>" `isInfixOf` s
                                  then stripSignature $ drop 3 $ dropWhile ((/=) '=') s
                                  else stripSignature $ drop 3 $ dropWhile ((/=) ':') s

-- | Strip unneeded whitespace from a signature, e.g. @String -> Map k a -> Int@ will be transformed
-- to @String->Map k a->Int@.

stripSignature 			:: String -> String
stripSignature 			= sep "->" . lsep "(" . rsep ")" . sep "." . sep "=>"
    where
    sep s 			= join s . map strip  . split s
    lsep s 			= join s . map stripl . split s
    rsep s 			= join s . map stripr . split s

