module Model where

import Control.Alt
import Data.Argonaut.Decode
import Data.Argonaut.Combinators
import Data.Either
import Prelude

data Message = Connect ConnectObject | Chat ChatObject

newtype ConnectObject = ConnectObject {
  nick :: String
}

newtype ChatObject = ChatObject {
  chatMsg :: String
}

newtype ClientChatMsg = ClientChatMsg {
  nick :: String,
  msg :: String    
}

instance decodeConnectMsg :: DecodeJson ConnectObject where
  decodeJson json = do
    obj <- decodeJson json
    n <- obj .? "nick"
    pure $ ConnectObject { nick: n }

instance decodeChatMsg :: DecodeJson ChatObject where
  decodeJson json = do
    obj <- decodeJson json
    n <- obj .? "chatMsg"
    pure $ ChatObject { chatMsg: n }

instance decodeClientMsg :: DecodeJson Message where
  decodeJson json = (Connect <$> decodeJson json) <|> (Chat <$> decodeJson json) <|> (Left "NONONO")
