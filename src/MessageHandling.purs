module MessageHandling where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console hiding (error)
import Control.Monad.Eff.Exception
import Control.Monad.Reader.Trans
import Data.Either
import Data.Argonaut.Core hiding (toString)
import Data.Argonaut.Parser
import Data.Argonaut.Printer
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import qualified Node.Buffer as Buffer
import qualified Node.Encoding as Encoding
import qualified Node.Datagram as UDP

import Prelude

import LoggingUtils
import Model

newtype MsgHandlerContext = MsgHandlerContext {
  socket :: UDP.Socket
}

type AppMsgHandler eff = ReaderT MsgHandlerContext (Eff eff)

sendMessage :: forall a eff. (EncodeJson a) => a -> UDP.Address -> UDP.Port -> AppMsgHandler (socket :: UDP.SOCKET, console :: CONSOLE | eff) Unit
sendMessage msg address port = do
  MsgHandlerContext { socket: socket } <- ask
  let jsonStr = printJson $ encodeJson msg
  let socketSendAction = sendStringToSocket socket address port jsonStr
  lift <<< catchException logError $ launchAff socketSendAction

sendStringToSocket :: forall eff. UDP.Socket -> UDP.Address -> UDP.Port -> String -> Aff (socket :: UDP.SOCKET | eff) Unit
sendStringToSocket socket address port msg =
  UDP.send buffer 0 (Buffer.size buffer) port address socket where
    buffer = Buffer.fromString msg Encoding.UTF8

parseIncomingMsg :: String -> Either String Message
parseIncomingMsg msg = jsonParser msg >>= (decodeJson :: Json -> Either String Message)
