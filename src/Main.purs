{-
  @license MIT
  Main.purs
-}

module Main
where

import Chat as Chat
import Chat.Model.Contact (Contact)
import Chat.Proact ((..), preuse)
import Chat.Proact (dispatch', spec) as P
import Contact as Contact
import ContactList as ContactList
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Except (Except, except, runExcept)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Array (snoc)
import Data.Foldable (find)
import Data.Foreign (readString, toForeign)
import Data.Lens (Lens', Traversal', (%=), (.=), use)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), fromJust, isNothing, maybe)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener) as D
import DOM.HTML (window) as D
import DOM.HTML.Event.EventTypes (unload) as D
import DOM.HTML.Window (document) as D
import DOM.HTML.Types (htmlDocumentToParentNode, windowToEventTarget) as D
import DOM.Node.ParentNode (QuerySelector(..), querySelector) as D
import DOM.Websocket.Event.EventTypes (onMessage) as D
import DOM.Websocket.Event.MessageEvent (data_) as D
import DOM.Websocket.WebSocket
  (URL(..), close, create, readMessageEvent, socketToEventTarget) as D
import Json (parseJSON)
import MessageList as MessageList
import Partial.Unsafe (unsafePartial)
import Prelude
import React (createClass, createFactory) as R
import ReactDOM (render) as R
import Unsafe.Coerce (unsafeCoerce)

main :: Eff (dom :: DOM) Unit
main =
  unsafePartial
    do
    window <- D.window
    socket <- D.create (D.URL "ws://localhost:3000") []
    D.addEventListener D.unload (D.eventListener $ const $ D.close socket) false
      $ D.windowToEventTarget window
    rDocument <- map D.htmlDocumentToParentNode $ D.document window
    rApp <- fromJust <$> D.querySelector (D.QuerySelector "#app") rDocument
    let element = flip R.createFactory { } $ R.createClass $ spec socket
    void $ R.render element rApp
  where
  spec socket =
    (P.spec socket Chat.empty Chat.chat)
      { componentDidMount = subscribeToSocket socket }

  subscribeToSocket socket this =
    D.addEventListener D.onMessage (D.eventListener onMessageEvent) false
      $ D.socketToEventTarget socket
    where
    _contactList = Chat._contactList .. ContactList._contactList

    _contactMap :: String -> Traversal' Chat.State Contact.State
    _contactMap cid = Chat._contactList .. ContactList._contactMap .. ix cid

    _local :: Lens' Chat.State (Maybe (Contact ()))
    _local = Chat._messageList .. MessageList._local

    _remote :: Lens' Chat.State (Maybe (Contact ()))
    _remote = Chat._messageList .. MessageList._remote

    exceptT :: forall m a e . Applicative m => Except e a -> ExceptT e m a
    exceptT = except <<< runExcept

    onMessageEvent event =
      (void <<< unsafeCoerceEff <<< runExceptT)
        do
        messageEvent <- exceptT $ D.readMessageEvent $ toForeign event
        message <- map parseJSON $ exceptT $ readString $ D.data_ messageEvent
        lift
          case message._type
            of
            "login" ->
              -- Set the local user.
              P.dispatch' socket this
                $ _local .= Just (unsafeCoerce message.body)
            "message" ->
              (P.dispatch' socket this <<< void <<< runMaybeT)
                do
                -- Find the contacts of the recipient and sender of the message.
                recipient <- findContact $ unsafeCoerce message.body.to
                sender <- findContact $ unsafeCoerce message.body.from
                local <- MaybeT $ use _local
                remote' <- lift $ use _remote
                let fromRemote remote = message.body.from == remote.cid
                let
                  addMessage messageList =
                    snoc
                      messageList
                      { body : unsafeCoerce message.body.body
                      -- A message is assumed to be read if it was send by the
                      -- user or by the remote contact.
                      , readFlag :
                        message.body.from == local.cid
                          || maybe false fromRemote remote'
                      , recipient : recipient
                      , sender : sender
                      }
                let
                  owner =
                    if sender.cid == local.cid
                      then recipient
                      else sender
                -- Add the message to the list associated with its remote user.
                lift
                  $ _contactMap owner.cid .. Contact._messageList %= addMessage
            "contactList" ->
              P.dispatch' socket this
                do
                -- If the remote user disconnected, set its status as offline.
                remote <- use (Chat._messageList .. MessageList._remote)
                when (checkOffline remote $ unsafeCoerce message.body)
                  $ Chat._messageList .. MessageList._offline .= true

                -- Update the contact list.
                _contactList .= unsafeCoerce message.body
            _ -> pure unit

    -- Checks if the remote contact disconnected from the chat.
    checkOffline :: Maybe (Contact ()) -> Array Contact.State -> Boolean
    checkOffline (Just remote) contactList =
      isNothing $ find (\contact -> contact.cid == remote.cid) contactList
    checkOffline Nothing _ = false

    findContact cid =
      do
      local <- MaybeT $ use _local
      if local.cid == cid
        then
          pure { cid : local.cid, name : local.name, picture : local.picture }
        else
          do
          remote <- MaybeT $ preuse (_contactMap cid)
          pure
            { cid : remote.cid, name : remote.name, picture : remote.picture }
