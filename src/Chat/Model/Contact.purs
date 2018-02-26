{-
  @license MIT
  Contact.purs
-}

module Chat.Model.Contact
  ( Contact
  , _cid
  , _name
  , _picture
  )
where

import Data.Lens (Lens', lens)

-- | A type synonym for a chat contact.
type Contact e =
  { cid :: String
  , name :: String
  , picture :: String
  | e
  }

-- | Gets or sets the contact's id.
_cid :: forall e . Lens' (Contact e) String
_cid = lens _.cid (_ { cid = _ })

-- | Gets or sets the contact's name.
_name :: forall e . Lens' (Contact e) String
_name = lens _.name (_ { name = _ })

-- | Gets or sets the contact's picture.
_picture :: forall e . Lens' (Contact e) String
_picture = lens _.picture (_ { picture = _ })
