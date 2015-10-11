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
import Data.Argonaut.Core hiding (toString)
import Data.Argonaut.Parser
import Data.Argonaut.Encode
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

newtype Member = Member {
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

runServer :: forall e h. UDP.Socket -> Eff (st :: ST h, console :: CONSOLE, socket :: UDP.SOCKET | e) Unit
runServer socket = do
  serverState <- newSTRef $ ServerState { members: Nil }
  let msgListener = createMessageDispatcher socket serverState
  runAff logError logListenStart $ UDP.onMessage msgListener socket

createMessageDispatcher :: forall h eff. UDP.Socket -> STRef h ServerState -> SocketMessageDispatcher (console :: CONSOLE, socket :: UDP.SOCKET, st :: ST h | eff) Unit
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

handleIncomingMsg :: forall e. ServerState -> UDP.RemoteAddressInfo -> Message -> AppMsgHandler (console :: CONSOLE, socket :: UDP.SOCKET | e) ServerState

handleIncomingMsg (ServerState state) sender (Connect (ConnectObject { nick: nick })) = do
  lift <<< log $ nick ++ " joined from " ++ show sender
  let newMember = Member { nick: nick, addr: sender }
  let newMembers = newMember : state.members
  sendNewMemberJoinedMsg nick newMembers
  let updatedState = ServerState $ state { members = newMembers }
  pure updatedState

handleIncomingMsg (ServerState state) sender (Chat (ChatObject { chatMsg: msg })) = do
  lift <<< log $ msg 
  let sendingMember = find (isSender sender) state.members
  case sendingMember of
       Just (Member { nick: n }) -> sendChatMsg n msg state.members
       Nothing -> lift <<< log $ "Unknown member"
  pure $ ServerState state

isSender :: UDP.RemoteAddressInfo -> Member -> Boolean
isSender (UDP.RemoteAddressInfo r) (Member { addr: (UDP.RemoteAddressInfo mr) }) = 
  r.address == mr.address && r.port == mr.port

sendChatMsg :: forall e. String -> String -> List Member -> AppMsgHandler (console :: CONSOLE, socket :: UDP.SOCKET | e) Unit
sendChatMsg nick msg members = traverse_ (sendToMember chatMsg) members where
  chatMsg = ClientChatMsg { nick: nick, msg: msg }

sendNewMemberJoinedMsg :: forall e. String -> List Member -> AppMsgHandler (console :: CONSOLE, socket :: UDP.SOCKET | e) Unit
sendNewMemberJoinedMsg nick members = traverse_ (sendToMember connectMsg) members where
   connectMsg = ConnectObject { nick: nick }

sendToMember :: forall a e. (EncodeJson a) => a -> Member -> AppMsgHandler (console :: CONSOLE, socket :: UDP.SOCKET | e) Unit
sendToMember msg (Member { addr: (UDP.RemoteAddressInfo memberAddress) }) = sendMessage msg memberAddress.address memberAddress.port 

logListenStart :: forall eff. Unit -> Eff (console :: CONSOLE | eff) Unit 
logListenStart _ = log "Listening for connections..."
