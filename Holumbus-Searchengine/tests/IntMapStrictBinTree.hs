module Main where

type Key = Int

data Tree v = Empty
            | Node !Key !v !(Tree v) !(Tree v)
            | Lt   !Key !v !(Tree v)
            | Gt   !Key !v           !(Tree v)
            | Leaf !Key !v
              deriving (Show)


mkNode :: Key -> v -> Tree v -> Tree v -> Tree v
mkNode k v Empty Empty = Leaf k v
mkNode k v Empty r     = Gt   k v   r
mkNode k v l     Empty = Lt   k v l
mkNode k v l     r     = Node k v l r

getNode :: Tree v -> Tree v
getNode t@(Node _ _ _ _) = t
getNode   (Lt   k v l  ) = Node k v l     Empty
getNode   (Gt   k v   r) = Node k v Empty r
getNode   (Leaf l v    ) = Node l v Empty Empty
getNode    Empty         = Empty

split' :: k -> Tree v -> (Maybe v, Tree v, Tree v)
split' k t = undefined

join' :: Maybe (Key, v) -> Tree v -> Tree v -> Tree v
join' = undefined

expose' :: Tree v -> Maybe (Tree v)
expose' Empty = Nothing
expose' t     = Just $ getNode t

lookup' :: k -> Tree v -> Maybe v
lookup' k t
    = v
      where
        (v, _l, _r) = split' k t

insert' :: Key -> v -> Tree v -> Tree v
insert' k v t
    = join' (Just (k, v)) l r
      where
        (v', l, r) = split' k t

insertWith' :: (v -> v -> v) -> Key -> v -> Tree v -> Tree v
insertWith' f k v t
    = join' (Just (k, f' v')) l r
      where
        (v', l, r)    = split' k t
        f' Nothing    =       v
        f' (Just v'') = f v'' v

delete' :: Key -> Tree v -> Tree v
delete' k t
    = join' Nothing l r
      where
        (_, l, r) = split' k t

main :: IO ()
main = return ()

