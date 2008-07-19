-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.MapReduce.TypeCheck
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.MapReduce.TypeCheck
(
  getFunctionParameters
, getTupleParameters
, getIOParameter
, getListParameter
, makeTuple
)
where

import Data.Typeable


-- ----------------------------------------------------------------------------
-- private helpers
-- ----------------------------------------------------------------------------

isFunction :: TypeRep -> Bool
isFunction f = (mkTyCon "->") == con
  where
  (con, _) = splitTyConApp f

isIO :: TypeRep -> Bool
isIO f = (mkTyCon "IO") == con
  where
  (con, _) = splitTyConApp f

isList :: TypeRep -> Bool
isList f = (mkTyCon "[]") == con
  where
  (con, _) = splitTyConApp f

isTuple :: TypeRep -> Bool
isTuple f = (mkTyCon ",") == con
  where
  (con, _) = splitTyConApp f


getFirstParam :: TypeRep -> TypeRep
getFirstParam = head . typeRepArgs 


getOtherParams :: TypeRep -> TypeRep
getOtherParams f = head $ makeTypeList (tail $ typeRepArgs f)


makeTypeList :: (Typeable a) => a -> [TypeRep]
makeTypeList l = maybe (error "unable to get parameterList") (id) (cast l)


foldTypeRepArgs :: (TypeRep -> Bool) -> TypeRep -> [TypeRep]
foldTypeRepArgs f t = reverse $ foldTypeRepArgs' t []
  where
  foldTypeRepArgs' t' ls
    | f t'      = foldTypeRepArgs' (getOtherParams t') ((getFirstParam t'):ls)
    | otherwise = t' : ls
        
foldTypeRepArg :: (TypeRep -> Bool) -> TypeRep -> TypeRep
foldTypeRepArg f t
  | f t       = getFirstParam t
  | otherwise = t




-- ----------------------------------------------------------------------------
-- public functions
-- ----------------------------------------------------------------------------

getFunctionParameters :: TypeRep -> [TypeRep]
getFunctionParameters t = foldTypeRepArgs (isFunction) t

getTupleParameters :: TypeRep -> [TypeRep]
getTupleParameters t = foldTypeRepArgs (isTuple) t

getIOParameter :: TypeRep -> TypeRep
getIOParameter t = foldTypeRepArg (isIO) t

getListParameter :: TypeRep -> TypeRep
getListParameter t = foldTypeRepArg (isList) t

makeTuple :: [TypeRep] -> TypeRep
makeTuple ls = mkTyConApp con ls
  where
  con = mkTyCon ","