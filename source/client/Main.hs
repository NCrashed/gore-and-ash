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
module Main(main) where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.Chan
import Control.Monad (forever)
import System.Exit (exitSuccess)

import Client.Network.System
import Client.Game.System
import Client.Graphics.System

exitMsg :: (ProcessId, String) -> Process ()
exitMsg (_, msg) = case msg of
    "exit" -> liftIO exitSuccess
    _      -> return ()  
    
main :: IO ()
main = do
    localTransport <- createTransport
    node <- newLocalNode localTransport initRemoteTable
    runProcess node $ do
        rootId <- getSelfPid
        _  <- initGameSystem rootId
        _  <- initGraphicsSystem rootId
        _  <- initNetworkSystem rootId
        forever $ receiveWait [match exitMsg]