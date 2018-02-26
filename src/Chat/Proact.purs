{-
  @license MIT
  Proact.purs
-}

module Chat.Proact
  ( Component
  , Dispatcher
  , EventDispatcher
  , EventHandler
  , (..)
  , _this
  , dispatch
  , dispatch'
  , preuse
  , send
  , spec
  , use'
  )
where

import Chat.Model.Message (Message)
import Chat.Program (ChatF(..), Idle(..))
import Control.Comonad.Env (Env, env)
import Control.Comonad.Store.Trans (StoreT(..))
import Control.Monad.IOSync (IOSync)
import Control.Monad.Reader (class MonadAsk, Reader, asks)
import Control.Monad.State (class MonadState, gets)
import Data.Lens (Fold, Getter, Iso, Lens', preview, view)
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First)
import Data.Tuple (Tuple(..))
import DOM.Websocket.WebSocket (WebSocket)
import Prelude
import Proact.React
  (ComponentT, Dispatcher, EventHandlerT, ReactEff, dispatch, spec) as P
import Proact.Monad.Class.MonadFree (class MonadFree, liftFree)
import React (ReactElement, ReactSpec, ReactThis, readState)

-- | An alias for Semigroupoid composition.
infixr 9 compose as ..

-- | A type synonym for a React Component with chat side-effects.
type Component s = P.ComponentT s Idle (Env WebSocket) ChatF (Reader WebSocket)

-- | A type synonym that executes the actions of an event handler.
type Dispatcher s a = P.Dispatcher s ChatF (Reader WebSocket) a

-- | A type synonym for a function dispatching React actions given an event
-- | argument.
type EventDispatcher v a = v -> P.ReactEff a

-- | A type synonym for an Event Handler with chat side-effects.
type EventHandler s = P.EventHandlerT s ChatF (Reader WebSocket)

-- | An alias for the identity isomorphism.
_this :: forall a b . Iso a b a b
_this = id

-- | Executes actions of an event handler over a React object.
dispatch
  :: forall s1 s2
   . WebSocket -> Lens' s1 s2 -> ReactThis { } s1 -> Dispatcher s2 Unit
dispatch socket _lens this eventHandler =
  do
  state <- readState this
  P.dispatch _lens Idle (start socket state) this eventHandler

-- | Executes actions of an event handler over a React object.
dispatch' :: forall s . WebSocket -> ReactThis { } s -> Dispatcher s Unit
dispatch' socket = dispatch socket _this

-- | An implementation of `preview` from the Data.Lens library working in the
-- | `MonadState` context.
preuse
  :: forall s t a b m . MonadState s m => Fold (First a) s t a b -> m (Maybe a)
preuse _get = gets (preview _get)

-- | The Free command to send a message through a WebSocket.
send :: forall m . MonadFree ChatF m => WebSocket -> Message () -> m Unit
send socket message = liftFree $ Send socket message unit

-- | Creates a `ReactSpec` from a React Component.
spec
  :: forall s e
   . WebSocket
  -> s
  -> Component s ReactElement
  -> ReactSpec { } s ReactElement e
spec socket state = P.spec Idle (start socket state)

-- | An implementation of `use` from the Data.Lens library working in the
-- | `MonadAsk` context.
use' :: forall s t a b m . MonadAsk s m => Getter s t a b -> m a
use' _get = asks (view _get)

-- The initial step of the interpreter Coalgebra.
start
  :: forall s . WebSocket -> s -> StoreT (Maybe s) (Env WebSocket) (IOSync Unit)
start socket state =
  StoreT $ Tuple (env socket $ const $ pure unit) $ Just state
