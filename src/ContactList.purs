{-
  @license MIT
  ContactList.purs
-}

module ContactList
  ( State
  , _contactList
  , _contactMap
  , _searchFilter
  , contactList
  , empty
  )
where

import Chat.Proact ((..), Component, EventDispatcher, use')
import Contact as Contact
import Control.Monad.Reader (ask)
import Data.Array (singleton, fromFoldable)
import Data.Foldable (all, any, foldr, elem)
import Data.Lens (Lens', (.=), filtered, lens, traversed)
import Data.Map (Map, filter, insert, member, values)
import Data.Monoid (mempty)
import Data.Profunctor (lmap)
import Data.String (Pattern(..), contains, split, toLower)
import Prelude
import Proact.React (dispatcher, focus) as P
import React (ReactElement) as R
import React.DOM (div, i, input, ul) as R
import React.DOM.Props
  (_type, className, onChange, onKeyUp, placeholder, value) as R
import Unsafe.Coerce (unsafeCoerce)

-- | A type synonym for the state of a Contact List component.
type State =
  { contactMap :: Map String Contact.State
  , searchFilter :: String
  }

-- | Gets or sets the map of contacts indexed by their id.
_contactMap :: Lens' State (Map String Contact.State)
_contactMap = lens _.contactMap (_ { contactMap = _ })

-- | Gets or sets the list of contacts.
_contactList :: Lens' State (Array Contact.State)
_contactList = lens _get _set
  where
  _get state = fromFoldable $ values state.contactMap

  _set state contactList_ =
    state { contactMap = foldr insertContact filteredMap contactList_ }
    where
    filteredMap = filter keepContact state.contactMap

    insertContact contact contactMap =
      if member contact.cid contactMap
        then contactMap
        else insert contact.cid (contact { messageList = [ ] }) contactMap

    keepContact mapContact = elem mapContact.cid idList

    idList = map (_.cid) contactList_

-- | Gets or sets the contact filter criteria.
_searchFilter :: Lens' State String
_searchFilter = lens _.searchFilter (_ { searchFilter = _ })

-- | The Contact List component.
contactList
  :: EventDispatcher Contact.State Unit -> Component State R.ReactElement
contactList onContactSelected =
  do
  state <- ask
  dispatcher <- map (..) P.dispatcher

  searchFilter <- use' _searchFilter
  contactsView <-
    P.focus (_contactList .. traversed .. filtered (contactFilter searchFilter))
      $ map singleton
      $ Contact.contact onContactSelected

  pure $ view dispatcher state contactsView
  where
  contactFilter "" _ = true
  contactFilter searchFilter contact =
    all containsFilter $ split' space searchFilter
    where
    containsFilter word =
      any (contains $ Pattern word) $ split' space contact.name

    space = Pattern " "

    split' pattern = map toLower <<< split pattern

  fromInputEvent event =
    { keyCode : (unsafeCoerce event).keyCode
    , text : (unsafeCoerce event).target.value
    }

  onSearchBoxChanged event = _searchFilter .= event.text

  onSearchBoxKeyUp event =
    if event.keyCode == 27
      then _searchFilter .= ""
      else pure unit

  view dispatcher state contactsView =
    R.div
      [ R.className "people-list" ]
      [
        R.div
          [ R.className "search" ]
          [
            R.input
              [ R._type "text"
              , R.placeholder "search"
              , R.value state.searchFilter
              ,
                R.onKeyUp
                  $ unsafeCoerce
                  $ lmap fromInputEvent
                  $ dispatcher onSearchBoxKeyUp
              ,
                R.onChange
                  $ unsafeCoerce
                  $ lmap fromInputEvent
                  $ dispatcher onSearchBoxChanged
              ]
              [ ]
          , R.i [ R.className "fa fa-search" ] [ ]
          ]
      , R.ul [ R.className "list" ] contactsView
      ]

-- | The initial state of the component.
empty :: State
empty =
  { contactMap : mempty
  , searchFilter : ""
  }
