module Main where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.ST
import Control.Monad.Reader.Trans
import qualified Control.Monad.Aff.Console as AConsole
import Data.Either
import Data.List
import Data.Maybe
import Data.Foldable (traverse_, find)
import qualified Node.Datagram as UDP
import qualified Node.Encoding as Encoding
import qualified Node.Buffer as Buffer

import MessageHandling 
import LoggingUtils
import qualified Chat as Chat

import Prelude

socketType = UDP.UDP4
encoding = Encoding.UTF8
listenPort = Just 62111
listenInterfaces = Nothing -- all interfaces

type SocketMessageDispatcher eff a = (Buffer.Buffer -> UDP.RemoteAddressInfo -> Eff eff a)

main = launchAff $ do
  socket <- UDP.createSocket socketType
  UDP.onError logError socket
  addrInfo <- UDP.bindSocket listenPort listenInterfaces socket
  AConsole.log $ show addrInfo
  liftEff $ (runST (runServer socket))

runServer :: forall h eff. UDP.Socket -> Eff (st :: ST h, console :: CONSOLE, socket :: UDP.SOCKET | eff) Unit
runServer socket = do
  serverState <- newSTRef Chat.initialState
  let msgListener = createMessageDispatcher socket serverState
  runAff logError logListenStart $ UDP.onMessage msgListener socket

createMessageDispatcher :: forall h eff. UDP.Socket -> STRef h Chat.ServerState -> SocketMessageDispatcher (console :: CONSOLE, socket :: UDP.SOCKET, st :: ST h | eff) Unit
createMessageDispatcher socket serverState = \buf rinfo -> do
  let msg = Buffer.toString encoding buf
  let parsedJson = parseIncomingMsg msg
  case parsedJson of
       (Left errMsg) -> log $ "Error parsing JSON: " ++ errMsg
       (Right msg) -> do
         currentState <- readSTRef serverState
         let handlerContext = MsgHandlerContext { socket: socket }
         let handler = Chat.handleIncomingMsg currentState rinfo msg
         newState <- runReaderT handler handlerContext
         writeSTRef serverState newState
         pure unit

logListenStart :: forall eff. Unit -> Eff (console :: CONSOLE | eff) Unit 
logListenStart _ = log "Listening for connections..."
