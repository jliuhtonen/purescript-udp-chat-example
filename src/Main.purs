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
import Data.Argonaut.Core hiding (toString)
import Data.Argonaut.Parser
import Data.Argonaut.Decode
import qualified Node.Datagram as UDP
import qualified Node.Encoding as Encoding
import qualified Node.Buffer as Buffer

import Model
import MessageHandling
import LoggingUtils

import Prelude

socketType = UDP.UDP4
encoding = Encoding.UTF8
listenPort = Just 62111
listenInterfaces = Nothing -- all interfaces

type SocketMessageDispatcher eff a = (Buffer.Buffer -> UDP.RemoteAddressInfo -> Eff eff a)

type Member = {
  nick :: String,
  addr :: UDP.RemoteAddressInfo
}

newtype ServerState = ServerState {
  members :: List Member    
}

main = launchAff $ do
  socket <- UDP.createSocket socketType
  UDP.onError logError socket
  addrInfo <- UDP.bindSocket listenPort listenInterfaces socket
  AConsole.log $ show addrInfo
  liftEff $ (runST (runServer socket))
  AConsole.log "aww yiss"

runServer :: forall e h. UDP.Socket -> Eff (st :: ST h, socket :: UDP.SOCKET, console :: CONSOLE) Unit
runServer socket = do
  serverState <- newSTRef $ ServerState { members: Nil }
  let msgListener = createMessageDispatcher socket serverState
  runAff logError logListenStart $ UDP.onMessage msgListener socket

createMessageDispatcher :: forall h eff. UDP.Socket -> STRef h ServerState -> SocketMessageDispatcher (console :: CONSOLE, st :: ST h) Unit
createMessageDispatcher socket serverState = \buf rinfo -> do
  let msg = Buffer.toString encoding buf
  let parsedJson = parseIncomingMsg msg
  case parsedJson of
       (Left errMsg) -> log $ "Error parsing JSON: " ++ errMsg
       (Right msg) -> do
         currentState <- readSTRef serverState
         let handlerContext = MsgHandlerContext { socket: socket }
         let handler = handleIncomingMsg currentState rinfo msg
         newState <- runReaderT handler handlerContext
         writeSTRef serverState newState
         pure unit

handleIncomingMsg :: forall e h. ServerState -> UDP.RemoteAddressInfo -> Message -> AppMsgHandler (console :: CONSOLE | e) ServerState

handleIncomingMsg (ServerState state) sender (Connect (ConnectObject { nick: nick })) = do
  lift <<< log $ nick ++ " joined from " ++ show sender
  let newMember = { nick: nick, addr: sender }
  let updatedState = ServerState $ state { members = newMember : state.members }
  pure updatedState

handleIncomingMsg (ServerState state) sender (Chat (ChatObject { chatMsg: msg })) = do
  lift <<< log $ msg 
  pure $ ServerState state

logListenStart :: forall eff. Unit -> Eff (console :: CONSOLE | eff) Unit 
logListenStart _ = log "Listening for connections..."
