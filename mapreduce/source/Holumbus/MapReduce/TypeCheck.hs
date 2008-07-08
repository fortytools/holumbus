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
  checkMapCombine
, checkMapReduce
, checkCombineReduce
, checkTypes
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


getFirstParam :: TypeRep -> TypeRep
getFirstParam = head . typeRepArgs 


getOtherParams :: TypeRep -> TypeRep
getOtherParams f = head $ makeTypeList (tail $ typeRepArgs f)


makeTypeList :: (Typeable a) => a -> [TypeRep]
makeTypeList l = maybe (error "unable to get parameterList") (id) (cast l)


getParamList :: (Typeable a) => a -> [TypeRep]
getParamList f = reverse $ foldTypeRepArgs (typeOf f)
  where


foldTypeRepArgs :: TypeRep -> [TypeRep]
foldTypeRepArgs t = foldTypeRepArgs' t []
  where
  foldTypeRepArgs' f' ls
    | isFunction f' = foldTypeRepArgs' (getOtherParams f') ((getFirstParam f'):ls)
    | otherwise     = f' : ls
    



-- ----------------------------------------------------------------------------
-- public functions
-- ----------------------------------------------------------------------------

    
checkMapCombine :: TypeRep -> TypeRep -> Bool
checkMapCombine = checkMapReduce
    

checkMapReduce :: TypeRep -> TypeRep -> Bool
checkMapReduce _ _ = True
--  where
--    mapArgs = foldTypeRepArgs mapFct
--    comArgs = foldTypeRepArgs comFct


checkCombineReduce :: TypeRep -> TypeRep -> Bool
checkCombineReduce _ _ = True


checkTypes :: TypeRep -> Maybe TypeRep -> TypeRep -> Bool
checkTypes _ _ _ = True