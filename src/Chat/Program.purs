{-
  @license MIT
  Program.purs
-}

module Chat.Program
  ( ChatF(..)
  , Idle(..)
  )
where

import Chat.Model.Message (Message)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.IOSync (IOSync)
import Data.Newtype (class Newtype)
import DOM.Websocket.WebSocket (WebSocket, sendString)
import Json (stringify)
import Prelude
import Proact.Functor.Pairing (class PairingM)

-- | Represents the chat commands used by components.
data ChatF a = Send WebSocket (Message ()) a

-- Represents the absence of actions.
newtype Idle a = Idle a

-- ChatF :: Functor
derive instance functorChatF :: Functor (ChatF)

-- Idle :: Functor
derive instance functorNewtype :: Newtype (Idle a) _

derive instance functorIdle :: Functor (Idle)

-- (Idle, ChatF, IOSync) :: PairingM
instance pairingMIdleChatFIOSync :: PairingM Idle ChatF IOSync
  where
  pairM p (Idle a) (Send socket message b) =
    do
    (liftEff <<< sendString socket <<< stringify)
      { _type : "message"
      , body :
        { body : message.body
        , from : message.sender.cid
        , to : message.recipient.cid
        }
      }
    p a b
