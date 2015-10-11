module Model where

import Control.Alt
import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Argonaut.Encode
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

instance encodeClientChatMsg :: EncodeJson ClientChatMsg where
  encodeJson (ClientChatMsg m) = 
    "nick" := m.nick ~> 
    "msg" := m.msg ~>
    jsonEmptyObject

instance decodeConnectMsg :: DecodeJson ConnectObject where
  decodeJson json = do
    obj <- decodeJson json
    n <- obj .? "nick"
    pure $ ConnectObject { nick: n }

instance encodeConnectMsg :: EncodeJson ConnectObject where
  encodeJson (ConnectObject r) = "nick" := r.nick ~> jsonEmptyObject 

instance decodeChatMsg :: DecodeJson ChatObject where
  decodeJson json = do
    obj <- decodeJson json
    n <- obj .? "chatMsg"
    pure $ ChatObject { chatMsg: n }

instance decodeClientMsg :: DecodeJson Message where
  decodeJson json = (Connect <$> decodeJson json) <|> (Chat <$> decodeJson json) <|> (Left "NONONO")
