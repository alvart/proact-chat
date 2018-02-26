{-
  @license MIT
  Chat.purs
-}

module Chat
  ( State
  , _contactList
  , _messageList
  , chat
  , empty
  )
where

import Chat.Model.Message (_readFlag) as Message
import Chat.Proact ((..), Component)
import Contact as Contact
import ContactList as ContactList
import Data.Lens (Lens', (.=), (^.), lens, traversed)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..))
import MessageList as MessageList
import Prelude
import Proact.React (dispatcher, focus') as P
import React (ReactElement) as R
import React.DOM (div) as R
import React.DOM.Props (className) as R

-- | A type synonym for the state of the Chat application.
type State =
  { contactList :: ContactList.State
  , messageList :: MessageList.State
  }

-- | Gets or sets the list of contacts.
_contactList :: Lens' State ContactList.State
_contactList = lens _.contactList (_ { contactList = _ })

-- | Gets the list of active messages.
_messageList :: Lens' State MessageList.State
_messageList = lens _get _set
  where
  _contactMessageList cid =
    _contactList .. ContactList._contactMap .. ix cid .. Contact._messageList

  _get state =
    case state.messageList.remote
      of
      Just remote ->
        state.messageList
          { messageList = state ^. _contactMessageList remote.cid }
      Nothing -> state.messageList { messageList = [ ] }

  -- Setter should technically update message list in the contact map but
  -- because this setter is never used to update the message list directly, a
  -- dummy implementation is provided instead.
  _set = _ { messageList = _ }

-- | The chat application.
chat :: Component State R.ReactElement
chat =
  do
  dispatcher <- map (..) P.dispatcher

  messageListView <- P.focus' _messageList MessageList.messageList
  contactListView <-
    P.focus' _contactList $ ContactList.contactList $ dispatcher onSelectContact

  pure $ view contactListView messageListView
  where
  _readFlag cid =
    _contactList
      .. ContactList._contactMap
      .. ix cid
      .. Contact._messageList
      .. traversed
      .. Message._readFlag

  onSelectContact contact =
    do
    -- Mark all messages of the contact as read.
    _readFlag contact.cid .= true

    -- Set the remote user.
    _messageList .. MessageList._remote .=
      Just { cid : contact.cid, name : contact.name, picture : contact.picture }

    -- Set the remote user as online.
    _messageList .. MessageList._offline .= false

  view contactListView messageListView =
    R.div
      [ R.className "container clearfix" ] [ contactListView, messageListView ]

-- | The initial state of the component.
empty :: State
empty =
  { contactList : ContactList.empty
  , messageList : MessageList.empty
  }
