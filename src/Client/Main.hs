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

import Network.Socket (withSocketsDo)
import Network.Transport
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import System.Environment
import Data.ByteString.Char8
import Control.Monad

main::IO()
main = withSocketsDo $ do
   [host, port, serverAddr] <- getArgs
   Right transport <- createTransport host port defaultTCPParameters
   Right endpoint  <- newEndPoint transport
   
   let addr = EndPointAddress (pack serverAddr)
   Right conn <- connect endpoint addr ReliableOrdered defaultConnectHints
   _ <- send conn [pack "Hello world"]
   close conn
   
   replicateM_ 3 $ receive endpoint >>= print
   
   closeTransport transport
  