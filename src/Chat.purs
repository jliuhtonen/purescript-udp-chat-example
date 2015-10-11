module Chat where

import Control.Monad.Eff.Console
import Data.Maybe
import Data.List
import Data.Foldable (traverse_, find)
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Control.Monad.Reader.Trans (lift)
import qualified Node.Datagram as UDP
import Prelude

import Model
import MessageHandling

newtype Member = Member {
  nick :: String,
  addr :: UDP.RemoteAddressInfo
}

newtype ServerState = ServerState {
  members :: List Member
}

initialState = ServerState { members: Nil }

handleIncomingMsg :: forall e. ServerState -> UDP.RemoteAddressInfo -> Message -> AppMsgHandler (console :: CONSOLE, socket :: UDP.SOCKET | e) ServerState

handleIncomingMsg (ServerState state) sender (Connect (ConnectObject { nick: nick })) = do
  lift <<< log $ nick ++ " joined from " ++ show sender
  let newMember = Member { nick: nick, addr: sender }
  let newMembers = newMember : state.members
  sendNewMemberJoinedMsg nick newMembers
  let updatedState = ServerState $ state { members = newMembers }
  pure updatedState

handleIncomingMsg (ServerState state) sender (Chat (ChatObject { chatMsg: msg })) = do
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
