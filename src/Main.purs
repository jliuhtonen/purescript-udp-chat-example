module Main where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console hiding (error)
import Control.Monad.Eff.Exception
import Control.Monad.ST
import Control.Monad.Reader.Trans
import qualified Control.Monad.Aff.Console as AConsole
import Data.Either
import Data.List
import Data.Maybe
import Data.Argonaut.Core hiding (toString)
import Data.Argonaut.Parser
import Data.Argonaut.Printer
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import qualified Node.Datagram as UDP
import qualified Node.Encoding as Encoding
import qualified Node.Buffer as Buffer

import Chat.JsonModel

import Prelude

socketType = UDP.UDP4
encoding = Encoding.UTF8
listenPort = Just 62111
listenInterfaces = Nothing -- all interfaces

newtype MsgHandlerContext = MsgHandlerContext {
  socket :: UDP.Socket
}

type SocketMessageHandler eff a = (Buffer.Buffer -> UDP.RemoteAddressInfo -> Eff eff a)
type AppMsgHandler eff = ReaderT MsgHandlerContext (Eff eff)

type Member = {
  nick :: String,
  addr :: UDP.RemoteAddressInfo
}

type ServerState = {
  members :: List Member    
}

main = launchAff $ do
  socket <- UDP.createSocket socketType
  UDP.onError logError socket
  addrInfo <- UDP.bindSocket listenPort listenInterfaces socket
  AConsole.log $ show addrInfo
  liftEff $ runServer socket
  AConsole.log "aww yiss"

runServer :: forall e. UDP.Socket -> Eff (socket :: UDP.SOCKET, console :: CONSOLE | e) Unit
runServer socket = runST do
  serverState <- newSTRef { members: Nil }
  let msgListener = \buf rinfo -> do
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
  runAff logError logListenStart $ UDP.onMessage msgListener socket

handleIncomingMsg :: forall e h. ServerState -> UDP.RemoteAddressInfo -> Command -> AppMsgHandler (console :: CONSOLE | e) ServerState

handleIncomingMsg state sender (Connect (ConnectObject { nick: nick })) = do
  lift <<< log $ nick ++ " joined from " ++ show sender
  let newMember = { nick: nick, addr: sender }
  let updatedState = state { members = newMember : state.members }
  pure updatedState

handleIncomingMsg state sender (Chat (ChatObject { chatMsg: msg })) = do
  lift <<< log $ msg 
  pure state

sendMessage :: forall a eff. (EncodeJson a) => a -> String -> Int -> AppMsgHandler (socket :: UDP.SOCKET, console :: CONSOLE | eff) Unit
sendMessage msg address port = do
  MsgHandlerContext { socket: socket } <- ask
  let jsonStr = printJson $ encodeJson msg
  lift $ log jsonStr
  let socketSendAction = sendStringToSocket socket address port jsonStr
  lift <<< catchException logError $ launchAff socketSendAction

sendStringToSocket :: forall eff. UDP.Socket -> String -> Int -> String -> Aff (socket :: UDP.SOCKET | eff) Unit
sendStringToSocket socket address port msg = 
  UDP.send buffer 0 (Buffer.size buffer) port address socket where
    buffer = Buffer.fromString msg encoding

parseIncomingMsg :: String -> Either String Command
parseIncomingMsg msg = jsonParser msg >>= (decodeJson :: Json -> Either String Command)

logListenStart :: forall eff. Unit -> Eff (console :: CONSOLE | eff) Unit 
logListenStart _ = log "Listening for connections..."

logMessage :: forall e. Buffer.Buffer -> UDP.RemoteAddressInfo -> Eff (console :: CONSOLE | e) Unit
logMessage msg rinfo = do 
  log $ show rinfo
  log $ Buffer.toString encoding msg

logError :: forall e. Error -> Eff (console :: CONSOLE | e) Unit
logError err = log $ "ERROR: " ++ message err
