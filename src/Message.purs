{-
  @license MIT
  Message.purs
-}

module Message
  ( State
  , _local
  , message
  )
where

import Control.Monad.Reader (ask)
import Chat.Model.Contact (Contact)
import Chat.Model.Message (WindowMessage)
import Chat.Proact (Component)
import Data.Lens (Lens', lens)
import Prelude
import React (ReactElement) as R
import React.DOM (div, li, span, text) as R
import React.DOM.Props (className) as R

-- | A type synonym for the state of a Message component.
type State = WindowMessage (local :: Contact ())

-- | Gets or sets the contact of the user.
_local :: Lens' State (Contact ())
_local = lens _.local (_ { local = _ })

-- | The Message component.
message :: Component State R.ReactElement
message = ask <#> view
  where
  view state =
    R.li
      [ R.className clearFixClass ]
      [
        R.div
          [ R.className messageDataClass ]
          [
            R.span
              [ R.className "message-data-name" ] [ R.text state.sender.name ]
          ]
      , R.div [ R.className messageBodyClass ] [ R.text state.body ]
      ]
    where
    clearFixClass =
      if othersMessage
        then ""
        else "clearfix"

    messageBodyClass =
      if othersMessage
        then "message other-message"
        else "message my-message float-right"

    messageDataClass =
      if othersMessage
        then "message-data"
        else "message-data align-right"

    othersMessage = state.sender.cid /= state.local.cid
