module ToTree where

data Tree v = Empty
            | Node v (Tree v) (Tree v)
              deriving (Show)

toTree :: [v] -> Tree v
toTree
    = toTr 0 Empty

toTr :: Int -> Tree v -> [v] -> Tree v
toTr _ t [] = t
toTr i t (x : xs)
    | null xs1  = t'
    | otherwise = toTr (i + 1) t' xs
    where
      (r, xs1) = scan i xs
      t'       = Node x t r

scan :: Int -> [v] -> (Tree v, [v])
scan 0 xs       = (Empty, xs)
scan _ []       = (Empty, [])
scan i xs
    | null xs1  = (l, [])
    | otherwise = (Node x l r, xs3)
    where
      (l,  xs1) = scan (i - 1) xs
      (x : xs2) = xs1
      (r,  xs3) = scan (i - 1) xs2
