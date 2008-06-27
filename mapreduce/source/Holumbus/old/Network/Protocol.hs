
module Holumbus.Network.Protocol where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.Binary
import qualified Data.Map as Map
import Network
import System.IO

import Holumbus.Task.TaskData

pingCmd :: String
pingCmd = "PING"

registerCmd :: String
registerCmd = "REGISTER"

unregisterCmd :: String
unregisterCmd = "UNREGISTER"

startTaskCmd :: String
startTaskCmd = "TASK_START"

completeTaskCmd :: String
completeTaskCmd = "TASK_COMPLETE"

successCode :: String
successCode = "OK"

failureCode :: String
failureCode = "FAIL"

type SocketData = (String, Integer)

getPortID :: SocketData -> PortID
getPortID = PortNumber . fromIntegral . snd 

getHostName :: SocketData -> HostName
getHostName = fst 

data Command   = Command (String, Parameters) deriving (Show)
data Parameters = Parameters (Map.Map String B.ByteString) deriving (Show)

instance Binary Command where
  put (Command (c,p)) = put c >> put p
  get = do
        c <- get
        p <- get
        return (Command (c,p)) 

instance Binary Parameters where
  put (Parameters m) = put m
  get = do
        m <- get        
        return (Parameters m)
        
mkCommand :: String -> Command
mkCommand c = Command (c, Parameters Map.empty)

addCommandParameter :: String -> B.ByteString -> Command -> Command
addCommandParameter p n (Command (c, Parameters m)) = Command (c, Parameters $ Map.insert p n m) 

getCommandString :: Command -> String
getCommandString (Command (c, _)) = c

getCommandParameter :: String -> Command -> Maybe B.ByteString
getCommandParameter p (Command (_, Parameters m)) = Map.lookup p m 

decodeParameter :: Binary a => Maybe B.ByteString -> Maybe a
decodeParameter (Just b) = Just (decode b)
decodeParameter Nothing  = Nothing

getDecodedParameter :: Binary a => String -> Command -> Maybe a
getDecodedParameter p c = decodeParameter $ getCommandParameter p c

getIntegerParameter :: String -> Command -> Maybe Integer
getIntegerParameter = getDecodedParameter

getStringParameter :: String -> Command -> Maybe String
getStringParameter = getDecodedParameter

getTaskDataParameter :: String -> Command -> Maybe TaskData
getTaskDataParameter = getDecodedParameter

putCommand :: Command -> Handle -> IO ()
putCommand c hdl
  = do
    --TODO better exception handling
    handle (\e -> putStrLn $ show e) $ do
      enc <- return (encode c)
      hPutStrLn hdl ((show $ B.length enc) ++ " ")
      B.hPut hdl enc

getCommand :: Handle -> IO (Command)
getCommand hdl
  = do
    --TODO better exception handling
    handle (\e -> do
                  putStrLn $ show e
                  return (failureCmd $ show e)) $ do
      -- read the Package
      pkg <- liftM words $ hGetLine hdl
      -- read the raw data 
      raw <- B.hGet hdl (read $ head pkg)
      return (decode raw)
    where
      failureCmd e = addCommandParameter "MSG" (encode e) $ mkCommand failureCode 
