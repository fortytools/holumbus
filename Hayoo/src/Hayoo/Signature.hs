{-# OPTIONS #-}

-- ------------------------------------------------------------

module Hayoo.Signature
where

import           Control.Arrow

import           Data.List

import           Holumbus.Crawler.Util  ( tokenize
                                        , match
                                        , split
                                        )

-- ------------------------------------------------------------

getSignature                    :: String -> String
getSignature                    = split "(.*(=>|::))?(\\s)*" >>> snd

-- | Strip redundant whitespace from a signature, e.g. @String -> Map k a -> Int@ will be transformed
-- to @String->Map k a->Int@.

stripSignature                  :: String -> String
stripSignature                  = tokenizeSignature >>> joinSignatureTokens

-- | Normalizes a Haskell signature, e.g. @String -> Int -> Int@ will be transformed to 
-- @a->b->b@. All whitespace will be removed from the resulting string.

normalizeSignature              :: String -> String
normalizeSignature              = tokenizeSignature >>> normSignatureIds >>> joinSignatureTokens

-- | Tokenize a signature string

tokenizeSignature               :: String -> [String]
tokenizeSignature               = tokenize sigToken
    where
    sigToken                    = "[\\]\\[(,)]|->|=>|\\*|" ++ sigIdent

ascSym                          :: String
ascSym                          = "[-!#$%&*+./<=>?@\\\\^|~]"

uniSym                          :: String
uniSym                          = "[\\p{S}\\p{P}-[(),;\\[\\]`{}_:'\"]]"

uniSymNoColon                   :: String
uniSymNoColon                   = "[\\p{S}\\p{P}-[(),;\\[\\]`{}_:'\"]]"

conSym                          :: String
conSym                          = ":(" ++ uniSymNoColon ++ "|" ++ uniSym ++ "{2,})"     -- somewhat tricky tricky (::) isn't a conSym

sigIdent                        :: String
sigIdent                        = "[\\p{L}#][\\p{L}\\{N}'_.]*"

haskIdent                       :: String
haskIdent                       = "[\\p{L}][\\p{L}\\{N}'_.]*"

varIdent                        :: String
varIdent                        = "[\\p{Ll}][\\p{L}\\{N}'_.]*"

typeIdent                       :: String
typeIdent                       = "[\\p{Lu}#][\\p{L}\\{N}'_.]*" -- "|" ++ conSym        -- but this requires latest hxt-8.5.3, beacuse of a parser error in uniSym expression

joinSignatureTokens             :: [String] -> String
joinSignatureTokens             = mapAccumL insBlank False >>> snd >>> concat
    where
    insBlank isId x             = (isId'
                                  , if isId && isId'
                                    then ' ' : x
                                    else       x
                                  )
        where
        isId'                   = match sigIdent x

normSignatureIds                :: [String] -> [String]
normSignatureIds                = mapAccumL renId ([], ['a'..]) >>> snd
    where
    renId ac@(env, ids) x
        | isId x                = case lookup x env of
                                  Nothing       -> ( ((x, head ids) : env, tail ids)
                                                   , [head ids]
                                                   )
                                  Just c        -> (ac, [c])
        | otherwise             = (ac, x)
        where
        isId                    = match varIdent

isSignature                     :: String -> Bool
isSignature                     = match $ anyWord ++ "(->|" ++ tupleType ++ "|" ++ listType ++ ")" ++ anyWord
    where
    anyWord                     = "(.|\\n|\\r)*"
    listType                    = "\\[" ++ anyWord ++ "\\]"
    tupleType                   = "\\((" ++ anyWord ++ "," ++ anyWord ++ ")?\\)"

-- ------------------------------------------------------------
