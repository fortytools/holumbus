{-# OPTIONS -XMultiParamTypeClasses -XFunctionalDependencies  -XFlexibleInstances #-}

module Data.Function.Selector
where

import Prelude 		hiding (id,(.))

import Control.Category

-- ------------------------------------------------------------

-- | A Selector is a pair of an access function and a modifying function
-- for reading and updating parts of a composite type

data Selector s a       = S { load  :: s -> a
			    , store :: a -> s -> s
			    }

update			:: Selector s a -> (a -> a) -> (s -> s)
update sel f s		= store sel x s
			  where
			  x = f . load sel $ s

-- | Alias for constructor S

mkSelector		:: (s -> a) -> (a -> s -> s) -> Selector s a
mkSelector		= S

instance Category Selector where
    id				= S { load  = id
				    , store = const {- \ x y -> x -}
				    }
    (S g2 s2) . (S g1 s1)	= S { load  = g2 . g1
				    , store = \ x s ->
				            let x1  = g1 s    in
				            let x1' = s2 x x1 in
				            s1 x1' s
				      }

-- | Selectors for pairs and 3-tuples: comp1, comp2, comp3,
-- this can be extended to n-tuples

class Comp1 s a | s -> a where  comp1	:: Selector s a
class Comp2 s a | s -> a where  comp2	:: Selector s a
class Comp3 s a | s -> a where  comp3	:: Selector s a


instance Comp1 (a, b) a where		comp1	= S { load  = fst
						    , store = \ x1 (_, x2) -> (x1, x2)
						    }

instance Comp2 (a, b) b where		comp2	= S { load  = snd
						    , store = \ x2 (x1, _) -> (x1, x2)
						    }

instance Comp1 (a, b, c) a where	comp1	= S { load  = \ (x1, _, _) -> x1
						    , store = \ x1 (_, x2, x3) -> (x1, x2, x3)
						    }

instance Comp2 (a, b, c) b where	comp2	= S { load  = \ (_, x2, _) -> x2
						    , store = \ x2 (x1, _, x3) -> (x1, x2, x3)
						    }

instance Comp3 (a, b, c) c where	comp3	= S { load  = \ (_, _, x3) -> x3
						    , store = \ x3 (x1, x2, _) -> (x1, x2, x3)
						    }

-- ------------------------------------------------------------

