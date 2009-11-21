{-
  devide image into coherent blocks
-}
devide :: Int -> [a] -> [(Int,[a])]
devide = devide' 0 []
  where
  devide' :: Int -> [(Int,[a])]-> Int -> [a] -> [(Int,[a])]
  devide' key xss n [] = xss
  devide' key xss n xs = devide' key' xss' n rest
    where
    key' = key + 1
    xss' = ((key,xs'):xss)
    (xs',rest) = splitAt n xs
