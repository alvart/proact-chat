{-
  @license MIT
  Message.purs
-}

module Chat.Model.Message
  ( Message
  , WindowMessage
  , _body
  , _readFlag
  , _recipient
  , _sender
  )
where

import Chat.Model.Contact (Contact)
import Data.Lens (Lens', lens)

-- | A type synonym for the messages interchanged in the chat.
type Message e =
  { body :: String
  , recipient :: Contact ()
  , sender :: Contact ()
  | e
  }

-- | A type synonym for the messages appearing in the chat window.
type WindowMessage e = Message (readFlag :: Boolean | e)

-- | Gets or sets the body of the message.
_body :: forall e . Lens' (Message e) String
_body = lens _.body (_ { body = _ })

-- | Gets or sets whether a message has been read or not.
_readFlag :: forall e . Lens' (WindowMessage e) Boolean
_readFlag = lens _.readFlag (_ { readFlag = _ })

-- | Gets or sets the recipient of the message.
_recipient :: forall e . Lens' (Message e) (Contact ())
_recipient = lens _.recipient (_ { recipient = _ })

-- | Gets or sets the sender of the message.
_sender :: forall e . Lens' (Message e) (Contact ())
_sender = lens _.sender (_ { sender = _ })
