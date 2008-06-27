
module Holumbus.Network.Client 
(
  sendRequest
)
where

import Control.Exception
--import Control.Monad
--import Data.List as L
import Network
import System.IO
import System.Posix

-- | Send the query to a server and merge the result with the global result.
sendRequest :: (Handle -> IO a) -> HostName -> PortID -> IO a
sendRequest f n p = 
  withSocketsDo $ do 
    installHandler sigPIPE Ignore Nothing
    
    --TODO exception handling
    --handle (\e -> do putStrLn $ show e return False) $
    bracket (connectTo n p) (hClose) (send)
    where    
    send hdl 
      = do
        hSetBuffering hdl NoBuffering
        f hdl
 
-- | Read the header from the handle and figure out if the request was successful. The left value
-- represents failure and contains the error message while the right value contains the length of
-- the returned data.
{-getResponse :: Handle -> IO (Either String Int)
getResponse hdl =
  do
  hdr <- liftM words $ hGetLine hdl
  if null hdr then return (Left "empty header") else
    if (head hdr) == successCode then 
      if null (tail hdr) then return (Right 0) 
      else return (Right $ read $ head $ tail hdr) 
    else 
      if null (tail hdr) then return (Left "no error message") 
      else return (Left $ L.intercalate " " (tail hdr))
-}