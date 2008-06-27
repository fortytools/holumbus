
module Holumbus.Master.WorkerRegistry 
(
    WorkerId
  , WorkerStatus(..)
  , WorkerRegistry
  , WorkerData
   
  , getWorkerId
  , getPortNumber
  , getHostName
  , getWorkerStatus
   
  , isLiving
  , getWorker
  , setWorker
  , updateWorkerLifeTime
  , emptyWorkerRegistry
  , createWorkerData
  , registerWorker
  , unregisterWorker
  , getWorkers
  , printWorkerList
)
where

--import Network

import System.IO

import qualified Data.Map as Map

data WorkerStatus   = New | Deleted | OK 
                      deriving (Show)

type WorkerId       = Integer
data WorkerData     = MkWorkerData 
  { 
    workerId   :: WorkerId       -- ^ WorkerId
  , portNumber :: Integer        -- ^ PortNumber of the worker socket
  , hostName   :: String         -- ^ Hostname
  , health     :: Integer        -- ^ Health
  , status     :: WorkerStatus   -- ^ Status
  } deriving (Show)
                      
type WorkerMap      = Map.Map WorkerId WorkerData
type WorkerRegistry = ( WorkerId, WorkerMap )

initialHealth :: Integer
initialHealth = 5

isLiving :: WorkerData -> Bool
isLiving wd = (health wd) > 0

decrHealth :: WorkerData -> WorkerData
decrHealth wd = wd { health = he' }
  where
    he' | he > 0    = he -1
        | otherwise = 0
    he = (health wd)

setLiving :: WorkerData -> WorkerData
setLiving wd = wd {health = initialHealth }

-- | initialize the worker Registry
emptyWorkerRegistry :: WorkerRegistry
emptyWorkerRegistry = ( 1, Map.empty )
    
-- | Get the next key for the workers (only private use)
nextKey :: WorkerId -> WorkerId
nextKey k = k + 1

-- | create a new worker data
createWorkerData :: Integer -> String -> WorkerData
createWorkerData po hn 
  = MkWorkerData 
    (-1)
    po
    hn
    initialHealth
    New

getWorkerId :: WorkerData -> WorkerId
getWorkerId = workerId

getPortNumber :: WorkerData -> Integer
getPortNumber = portNumber

getHostName :: WorkerData -> String
getHostName = hostName

getWorkerStatus :: WorkerData -> WorkerStatus
getWorkerStatus = status

-- | write the correct id in the worker data (only private use)
updateWorkerId :: WorkerId -> WorkerData -> WorkerData
updateWorkerId idx wd = wd { workerId = idx }

-- | registers a worker
registerWorker :: WorkerData -> WorkerRegistry -> ( WorkerId, WorkerRegistry )
registerWorker d (k, idx) = ( k, ( nk, Map.insert k nd idx ) )
  where nk = nextKey k
        nd = updateWorkerId k d

getWorker :: WorkerId -> WorkerRegistry -> Maybe WorkerData
getWorker i (_, m) = Map.lookup i m

setWorker :: WorkerData -> WorkerRegistry -> WorkerRegistry
setWorker d (m, reg) = (m, Map.insert (workerId d) d reg)

updateWorkerLifeTime :: [(WorkerId, Bool)] -> WorkerRegistry -> WorkerRegistry
updateWorkerLifeTime l reg = foldr updateLifeTime reg l
  where
  updateLifeTime (k, b) r
    | b == True 
      = case curWorker of
          (Just w) -> setWorker (setLiving w) r
          _        -> r
    | otherwise 
      = case curWorker of
          (Just w) -> setWorker (decrHealth w) r
          _        -> r
    where
      curWorker = getWorker k r
      
-- | unregisters a worker
unregisterWorker :: WorkerId -> WorkerRegistry -> WorkerRegistry
unregisterWorker k (n, idx) = (n, Map.delete k idx)

getWorkers :: WorkerRegistry -> [WorkerData]
getWorkers ( _, m ) = map snd $ Map.toList m

-- | print the registry
printWorkerList :: WorkerRegistry -> IO ()
printWorkerList (_, idx)
  = do
    if (Map.null idx) then
      putStrLn "no workers registered"
      else
      do
        putStrLn "Workers:"
        printWorkerEntry $ Map.toList idx
    where 
      printWorkerEntry [] = return ()
      printWorkerEntry (x : xs) 
        = do
            putStrLn (show x)
            printWorkerEntry xs