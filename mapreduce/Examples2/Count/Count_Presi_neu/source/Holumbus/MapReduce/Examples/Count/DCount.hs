module Holumbus.MapReduce.Examples.Count.DCount
(
   countMap
 , countReduce
 , K1
 , V1
)
where

import Holumbus.MapReduce.Examples.Count.SimpleDMapReduceIO
import Holumbus.MapReduce.Types
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as B
import GHC.Int (Int64)
import Data.Word (Word8)
import qualified Data.List as L

type Options = String
type K1 = Int
type V1 = C.ByteString
type K2 = Int
type V2 = C.ByteString
type V3 = V4
type V4 = Int64

{-

type MapFunction a k1 v1 k2 v2 = ActionEnvironment -> a -> k1 -> v1 -> IO [(k2, v2)]
-}
countMap :: MapFunction Options K1 V1 K2 V2
countMap env term key bs = do
  return [(newkey, newvalue)]
  where
  newkey = case (td_PartValue . ae_TaskData $ env) of
        (Just n') -> mod key n'
        Nothing   -> key 
 
  newvalue = L.foldl' f B.empty (C.words bs)
  
  f result bs = if (bs==term') then result'  else result
    where
    result' = (B.cons' w0 result)

  term' = C.pack term

  w0 = fromIntegral 0

{-

type ReduceFunction a k2 v2 v3 = ActionEnvironment -> a -> k2 -> [v2] -> IO (Maybe v3)
-}
countReduce :: ReduceFunction Options K2 V2 V4
countReduce _env _opts _k2 = return . Just . sum . map C.length
