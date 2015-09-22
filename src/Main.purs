module Main where

import qualified Control.Monad.Eff as E
import qualified Control.Monad.Eff.Console as EC
import qualified Node.Encoding as Encoding
import qualified Node.Buffer as Buffer
import Control.Monad.Aff
import Control.Monad.Aff.Console
import Data.Maybe
import Prelude

import Datagram.UDP
import qualified Datagram.UDP.Aff as U

socketType = "udp4"
encoding = Encoding.UTF8

printMessage :: forall e. Buffer.Buffer -> RemoteAddressInfo -> E.Eff (console :: EC.CONSOLE | e) Unit
printMessage msg rinfo = do 
    EC.log $ show rinfo
    EC.log $ Buffer.toString encoding msg

main = launchAff $ do
  socket <- U.createSocket socketType
  addrInfo <- U.bind (Just 62111) Nothing socket
  log $ show addrInfo
  U.onMessage printMessage socket
  let buffer = Buffer.fromString "FOO" encoding
  U.send buffer 0 (Buffer.size buffer) 41234 "127.0.0.1" socket
  log "aww yiss"
