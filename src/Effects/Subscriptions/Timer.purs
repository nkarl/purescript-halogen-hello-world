module Effects.Subscriptions.Timer where

import Prelude
import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)

import Halogen as H
import Halogen.HTML as HH
import Halogen.Subscription as HS

{--
  NOTE: there is 1 primary action: a numerical update on a clock `Tick`
--}
data Action
  = Initialize
  | Tick

type State = Int

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where
  initialState _ = 0

render :: forall m. State -> H.ComponentHTML Action () m
render seconds = HH.text ("You have been here for " <> show seconds <> " seconds.")

-- NOTE: can add a button to end the subscription, and thus the timer.

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of -- point free notation
  -- 2. subscribes the emitter (given the `Tick` action) to the event cycle object
  Initialize -> do
    _ <- H.subscribe =<< timer Tick
    pure unit
  -- 3. upon receiving a notification, we handle the action approrpiately.
  Tick -> H.modify_ \s -> s + 1

timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  -- 1. first creates an event cycle object which has an emitter and a listener
  { emitter, listener } <- H.liftEffect $ HS.create
  -- 2. forks a new fiber (that runs forever) to notify the listener every 1 second.
  _ <-
    H.liftAff $ Aff.forkAff $ forever do
      Aff.delay $ Milliseconds 1000.0
      H.liftEffect $ HS.notify listener val
  pure emitter
