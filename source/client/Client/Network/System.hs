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
module Client.Network.System(
      initNetworkSystem
    ) where
    
import qualified Control.Distributed.Process as Local
import Network.Socket (withSocketsDo)
import Network.Transport
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import System.Environment
import Data.ByteString.Char8
import Control.Monad

initNetworkSystem :: Local.ProcessId -> Local.Process Local.ProcessId
initNetworkSystem _ = Local.spawnLocal $ Local.liftIO $ withSocketsDo $ do
   [host, port, serverAddr] <- getArgs
   Right transport <- createTransport host port defaultTCPParameters
   Right endpoint  <- newEndPoint transport
   
   let addr = EndPointAddress (pack serverAddr)
   Right conn <- connect endpoint addr ReliableOrdered defaultConnectHints
   _ <- send conn [pack "Hello world"]
   close conn
   
   replicateM_ 3 $ receive endpoint >>= print
   
   closeTransport transport