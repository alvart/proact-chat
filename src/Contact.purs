{-
  @license MIT
  Contact.purs
-}

module Contact
  ( State
  , _messageList
  , contact
  )
where

import Chat.Model.Contact (Contact)
import Chat.Model.Message (WindowMessage)
import Chat.Proact (Component, EventDispatcher)
import Control.Monad.Reader (ask)
import Data.Array ((:))
import Data.Foldable (sum)
import Data.Lens (Lens', lens)
import Prelude
import React (ReactElement) as R
import React.DOM (a, div, i, img, li, span, text) as R
import React.DOM.Props (alt, className, onClick, src, width) as R

-- | A type synonym for the state of a Contact component.
type State = Contact (messageList :: Array (WindowMessage ()))

-- | Gets or sets the messages from a contact.
_messageList :: Lens' State (Array (WindowMessage ()))
_messageList = lens _.messageList (_ { messageList = _ })

-- | The Contact component.
contact :: EventDispatcher State Unit -> Component State R.ReactElement
contact onContactSelected = ask <#> view
  where
  view state =
    R.li
      [ R.className "clearfix" ]
      [
        R.a
          [ R.onClick $ const $ onContactSelected state ]
          [ R.img [ R.alt "avatar", R.src state.picture, R.width "50" ] [ ]
          , R.div [ R.className "about" ] aboutPanel
          ]
      ]
    where
    aboutPanel =
      R.span [ R.className "name" ] [ R.text state.name ]
        :
          if unreadCount > 0
            then
              [
                R.div
                  [ R.className "status" ]
                  [ R.i [ R.className "fa fa-envelope new-msg" ] [ ]
                  ,
                    R.text
                      if unreadCount > 99
                      then "+99 new messages"
                      else show unreadCount <> " new messages"
                  ]
              ]
            else [ ]

    readCounter message =
      if message.readFlag
        then 0
        else 1

    unreadCount = sum $ map readCounter state.messageList
