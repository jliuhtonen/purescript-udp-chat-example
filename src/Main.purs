module Main where

import Control.Alt
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.ST
import qualified Control.Monad.Eff.Console as EC
import Control.Monad.Eff.Exception
import qualified Node.Encoding as Encoding
import Node.Buffer (Buffer(), toString)
import Control.Monad.Aff
import Control.Monad.Aff.Console
import Data.Either
import Data.List
import Data.Argonaut.Core hiding (toString)
import Data.Argonaut.Parser
import Data.Argonaut.Printer
import Data.Argonaut.Decode
import Prelude
import Data.Maybe
import qualified Node.Datagram as U
import Chat.JsonModel

socketType = U.UDP4
encoding = Encoding.UTF8
listenPort = Just 62111
listenInterfaces = Nothing -- all interfaces

newtype Member = Member {
    nick :: String,
    addr :: U.RemoteAddressInfo
}

newtype ServerState = ServerState {
    members :: List Member    
}

main = launchAff $ do
    socket <- U.createSocket socketType
    U.onError logError socket
    addrInfo <- U.bindSocket listenPort listenInterfaces socket
    log $ show addrInfo
    liftEff $ runServer socket
    log "aww yiss"

runServer :: forall e h. U.Socket -> Eff (socket :: U.SOCKET, console :: EC.CONSOLE | e) Unit
runServer socket = runST do
    serverState <- newSTRef { members: Nil }
    let msgListener = \buf rinfo -> do
            let msg = toString encoding buf
            let parsedJson = parseIncomingMsg msg
            case parsedJson of
                 (Left errMsg) -> EC.log $ "Error parsing JSON: " ++ errMsg
                 (Right (Chat (ChatObject o))) -> EC.log o.chatMsg
                 (Right (Connect (ConnectObject { nick: nick }))) -> EC.log nick
    runAff logError logListenStart $ U.onMessage msgListener socket

parseIncomingMsg :: String -> Either String Command
parseIncomingMsg msg = jsonParser msg >>= (decodeJson :: Json -> Either String Command)

logListenStart :: forall eff. Unit -> Eff (console :: EC.CONSOLE | eff) Unit 
logListenStart _ = EC.log "Listening for connections..."

logMessage :: forall e. Buffer -> U.RemoteAddressInfo -> Eff (console :: EC.CONSOLE | e) Unit
logMessage msg rinfo = do 
    EC.log $ show rinfo
    EC.log $ toString encoding msg

logError :: forall e. Error -> Eff (console :: EC.CONSOLE | e) Unit
logError err = EC.log $ "ERROR: " ++ message err
