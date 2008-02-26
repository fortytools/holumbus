-- ----------------------------------------------------------------------------

{- |
  Module     : BinaryTest
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The some unit tests for the binary persistence.

-}

-- ----------------------------------------------------------------------------

module SampleData 
(
  -- * Sample data structures
  sampleIndex1
  , sampleResult1
  , sampleDocs1
)
where

import Holumbus.Index.Common
import Holumbus.Index.Inverted
import Holumbus.Index.Documents
import Holumbus.Query.Result

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import qualified Holumbus.Data.StrMap as SM
import qualified Holumbus.Data.DiffList as DL

sampleDocs1 :: Documents Int
sampleDocs1 = Documents
             (IM.fromList [ (1, (Document "doc1" "uri1" Nothing))
                          , (2, (Document "doc2" "uri2" Nothing))
                          , (3, (Document "doc3" "uri3" Nothing))
                          ])
             (M.fromList [("uri1", 1), ("uri2", 2), ("uri3", 3)])
             3

sampleIndex1 :: InvIndex
sampleIndex1 = InvIndex parts
  where
  parts = M.fromList [("context1", context1), ("context2", context2), ("context3", context3)]
  context1 = SM.fromList [ ("word1", IM.fromList [(1, DL.fromList [12,23,43]), (2, DL.fromList [10,2,4])])
                         , ("word2", IM.fromList [(3, DL.fromList [15,33,45]), (2, DL.fromList [1,32,94])])
                         , ("word3", IM.fromList [(3, DL.fromList [18,43,15]), (1, DL.fromList [18,82,64])])
                         ]
  context2 = SM.fromList [ ("word2", IM.fromList [(1, DL.fromList [29,63,73]), (2, DL.fromList [51,28,45])])
                         , ("word4", IM.fromList [(2, DL.fromList [5,65,47]), (3, DL.fromList [13,32,14])])
                         , ("word5", IM.fromList [(3, DL.fromList [8,48,55])])
                         ]
  context3 = SM.fromList [ ("word2", IM.fromList [(1, DL.fromList [24,60,77]), (3, DL.fromList [21,26,25])])
                         , ("word5", IM.fromList [(3, DL.fromList [86,78,35])])
                         ]

sampleResult1 :: Result Int
sampleResult1 = Result dh wh
  where
  dh = IM.fromList [(1, (DocInfo (Document "" "" Nothing) 1.0, dch1))
                   , (2, (DocInfo (Document "doc2" "uri2" Nothing) 2.0, dch2))
                   , (3, (DocInfo (Document "doc3" "uri3" Nothing) 3.0, dch3))
                   ]
  dch1 =  M.fromList [ ("context1", M.fromList [("word1", IS.fromList [12,23,43]), ("word2", IS.fromList [15,33,45])]) 
                     , ("context2", M.fromList [("word3", IS.fromList [86,78,35]), ("word4", IS.fromList [21,26,25])])
                     ]
  dch2 =  M.fromList [ ("context1", M.fromList [("word1", IS.fromList [12,23,43]), ("word2", IS.fromList [15,33,45])]) 
                     , ("context3", M.fromList [("word3", IS.fromList [8,48,55]), ("word4", IS.fromList [21,26,25])])
                     , ("context4", M.fromList [("word3", IS.fromList [86,78,35,1,32,94])])
                     ]
  dch3 =  M.fromList [ ("context1", M.fromList [("word1", IS.fromList [12,23,43]), ("word2", IS.fromList [15,33,45])]) 
                     , ("context2", M.fromList [("word3", IS.fromList [86,78,35]), ("word4", IS.fromList [13,32,14])])
                     , ("context3", M.fromList [("word3", IS.fromList [8,48,55]), ("word4", IS.fromList [10,2,4])])
                     , ("context4", M.fromList [("word3", IS.fromList [18,82,64])])
                     ]

  wh = M.fromList [ ("word1", (WordInfo ["wor"] 1.0, wch1)), ("word2", (WordInfo ["wor","word"] 2.0, wch2))
                  , ("word3", (WordInfo ["wor","word"] 3.0, wch3)), ("word4", (WordInfo ["wor"] 4.0, wch4))]
  wch1 = M.fromList [ ("context1", IM.fromList [(1, IS.fromList [12,23,43]), (2, IS.fromList [12,23,43]), (3, IS.fromList [12,23,43])])]
  wch2 = M.fromList [ ("context1", IM.fromList [(1, IS.fromList [15,33,45]), (2, IS.fromList [15,33,45]), (3, IS.fromList [15,33,45])])]
  wch3 = M.fromList [ ("context2", IM.fromList [(1, IS.fromList [86,78,35]), (3, IS.fromList [86,78,35])])
                    , ("context3", IM.fromList [(2, IS.fromList [8,48,55]), (3, IS.fromList [8,48,55])])
                    , ("context4", IM.fromList [(2, IS.fromList [86,78,35,1,32,94]), (3, IS.fromList [18,82,64])])
                    ]
  wch4 = M.fromList [ ("context2", IM.fromList [(1, IS.fromList [21,26,25]), (3, IS.fromList [13,32,14])])
                    , ("context3", IM.fromList [(2, IS.fromList [21,26,25]), (3, IS.fromList [10,2,4])])
                    ]
