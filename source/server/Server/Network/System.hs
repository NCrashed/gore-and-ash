-- Copyright 2013 Anton Gushcha
--    This file is part of Gore&Ash.
--
--    Gore&Ash is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    Gore&Ash is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with Gore&Ash.  If not, see <http://www.gnu.org/licenses/>.
module Server.Network.System(
     initNetworkSystem
    ) where
    
import Network.Socket (withSocketsDo)
import Network.Transport
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import qualified Control.Distributed.Process as Local
import Control.Concurrent
import Data.Map
import Control.Exception
import System.Environment

initNetworkSystem :: Local.ProcessId -> Local.Process Local.ProcessId
initNetworkSystem _ = Local.spawnLocal $ Local.liftIO $ withSocketsDo $ do
  [host, port]    <- getArgs
  serverDone      <- newEmptyMVar
  Right transport <- createTransport host port defaultTCPParameters
  Right endpoint  <- newEndPoint transport
  _ <- forkIO $ echoServer endpoint serverDone 
  putStrLn $ "Echo server started at " ++ show (address endpoint)
  readMVar serverDone `onCtrlC` closeTransport transport
  
echoServer :: EndPoint -> MVar () -> IO ()
echoServer endpoint serverDone = go empty
    where
        go :: Map ConnectionId (MVar Connection) -> IO ()
        go cs = do
            event <- receive endpoint
            case event of
                ConnectionOpened cid rel addr -> do
                    connMVar <- newEmptyMVar
                    _ <- forkIO $ do
                        Right conn <- connect endpoint addr rel defaultConnectHints
                        putMVar connMVar conn
                    go (insert cid connMVar cs)
                Received cid payload -> do
                    _ <- forkIO $ do
                        conn <- readMVar (cs ! cid)
                        _ <- send conn payload
                        return ()
                    go cs
                ConnectionClosed cid -> do 
                    _ <- forkIO $ do
                        conn <- readMVar (cs ! cid)
                        close conn
                    go (delete cid cs)
                EndPointClosed -> do
                    putStrLn "Echo server exiting!"
                    putMVar serverDone ()
                _ -> go cs
                    
onCtrlC :: IO a -> IO () -> IO a
p `onCtrlC` q = catchJust isUserInterrupt p ( const $ q >> p `onCtrlC` q )
    where
        isUserInterrupt :: AsyncException -> Maybe ()
        isUserInterrupt UserInterrupt = Just ()
        isUserInterrupt _             = Nothing           