{-
  @license MIT
  MessageList.purs
-}

module MessageList
  ( State
  , _local
  , _newMessageText
  , _offline
  , _remote
  , empty
  , messageList
  )
where

import Chat.Proact ((..), Component, send)
import Chat.Model.Contact (Contact)
import Chat.Model.Message (WindowMessage)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Data.Array (singleton)
import Data.Lens (Iso', Lens', (.=), iso, lens, traversed)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lmap)
import Message as Message
import Prelude
import Proact.React (dispatcher, focus) as P
import React (ReactElement) as R
import React.DOM (div, img, span', text, textarea, ul') as R
import React.DOM.Props
  ( alt
  , className
  , disabled
  , onChange
  , onKeyUp
  , placeholder
  , src
  , rows
  , value
  , width
  ) as R
import Unsafe.Coerce (unsafeCoerce)

-- | A type synonym for the state of a Message List component.
type State =
  { local :: Maybe (Contact ())
  , messageList :: Array (WindowMessage ())
  , newMessageText :: String
  , offline :: Boolean
  , remote :: Maybe (Contact ())
  }

-- | The initial state of the component.
empty :: State
empty =
  { local : Nothing
  , messageList : [ ]
  , newMessageText : ""
  , offline : false
  , remote : Nothing
  }

-- | Gets or sets the contact chatting.
_local :: Lens' State (Maybe (Contact ()))
_local = lens _.local (_ { local = _ })

-- | Gets or sets the messages of the chatting window.
_messageList :: Lens' State (Array (WindowMessage ()))
_messageList = lens _.messageList (_ { messageList = _ })

-- | Gets or sets the new message to be sent.
_newMessageText :: Lens' State String
_newMessageText = lens _.newMessageText (_ { newMessageText = _ })

-- | Gets or sets whether the contact on the other end is offline or not.
_offline :: Lens' State Boolean
_offline = lens _.offline (_ { offline = _ })

-- | Gets or sets the contact chatting on the other end.
_remote :: Lens' State (Maybe (Contact ()))
_remote = lens _.remote (_ { remote = _ })

-- | The message list component.
messageList :: Component State R.ReactElement
messageList =
  do
  state <- ask
  socket <- lift ask
  dispatcher <- map (..) P.dispatcher

  let local' = state.local
  let remote = state.remote

  messageView <-
    case local'
      of
      Just local ->
        P.focus (_messageList .. traversed .. _myMessage local)
          $ map singleton Message.message
      Nothing -> pure [ ]

  pure $ view socket dispatcher state local' remote messageView
  where
  _myMessage :: Contact () -> Iso' (WindowMessage ()) Message.State
  _myMessage local = iso windowToState stateToWindow
    where
    stateToWindow state =
      { body : state.body
      , recipient : state.recipient
      , sender : state.sender
      , readFlag : state.readFlag
      }

    windowToState window =
      { body : window.body
      , local : local
      , recipient : window.recipient
      , sender : window.sender
      , readFlag : window.readFlag
      }

  view socket dispatcher state (Just local) (Just remote) messageView =
    R.div
      [ R.className "chat" ]
      [
        R.div
          [ R.className "chat-header clearfix" ]
          [ R.img [ R.src remote.picture, R.alt "avatar", R.width "60" ] [ ]
          ,
            R.div
              [ R.className "chat-about" ]
              [
                R.div
                  [ R.className "chat-with" ]
                  [ R.text "Chat with ", R.span' [ R.text remote.name ] ]
              ]
          ]
      , R.div [ R.className "chat-history" ] [ R.ul' messageView ]
      ,
        R.div
          [ R.className "chat-message clearfix" ]
          [
            R.textarea
              [ R.disabled state.offline
              ,
                R.onChange
                  $ unsafeCoerce
                  $ lmap fromInputEvent
                  $ dispatcher onMessageChanged
              ,
                R.onKeyUp
                  $ unsafeCoerce
                  $ lmap fromInputEvent
                  $ dispatcher onMessageEnter
              , R.placeholder "Type your message"
              , R.rows 3
              , R.value state.newMessageText
              ]
              [ ]
          ]
      ]
    where
    fromInputEvent event =
      { keyCode : (unsafeCoerce event).keyCode
      , text : (unsafeCoerce event).target.value
      }

    onMessageChanged event = _newMessageText .= event.text

    onMessageEnter event =
      -- Pressing `Enter` sends the message.
      if event.keyCode == 13 && event.text /= ""
        then
          do
          _newMessageText .= ""
          (send socket)
            { body : event.text
            , recipient : unsafeCoerce remote
            , sender : unsafeCoerce local
            }
        -- Pressing `Esc` clears the input control.
        else if event.keyCode == 27
        then _newMessageText .= ""
        else pure unit
  view _ _ _ _ _ _ =
    R.div
      [ R.className "chat" ]
      [
        R.div
          [ R.className "chat-filler" ]
          [ R.span' [ R.text "No active conversations." ] ]
      ]
