module Core.Pattern.Input.Parent01 where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..))
import Effect.Aff                                                   as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen                                                      as H
import Halogen.HTML                                                 as HH
import Halogen.Subscription                                         as HS
import Type.Proxy (Proxy(..))

import Core.Pattern.Input.Child.Node01                              as Child

type State = { count :: Int }

data Action = Initialize | Increment

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

  initialState :: i -> State
  initialState _ = { count: 0 }

  --render :: State -> HH.HTML (H.ComponentSlot Slots m Action) Action
  render :: State -> H.ComponentHTML Action Child.Slots m
  render { count } =
    HH.div_
      [ HH.slot_ (Proxy :: Proxy "component") unit Child.component { label: count }
      ]

  handleAction :: Action -> H.HalogenM State Action Child.Slots o m Unit
  handleAction = case _ of

                     Initialize ->
                       do { emitter, listener } <- H.liftEffect HS.create
                          void $ H.subscribe emitter
                          void $ H.liftAff $ Aff.forkAff $ forever
                            do Aff.delay $ Milliseconds 1000.0
                               H.liftEffect $ HS.notify listener Increment
                      
                     Increment ->
                       H.modify_ \state -> state { count = state.count + 1 }

{--
  This example shows how to implement the async pub-sub pattern for a parent-child pipeline.

  We separate partition the logic:
    - Child takes care of rendering.
    - Parent takes care of async effects.

  In parent, we have 2 action variants:
    1. origin variant: Initialize
    2. update variant: Increment

  The origin/initial variant is responsible for kickstarting the async state machine by creating a new
  subscription structure. This structure contains a pair of emitter and listener.

  Halogen then subscribes to the emitter and then forks a new fiber for the listener to handle async events.
  There is only one kind of async event here: delay for 1 second and then notify the listener.
  In other words, the rules for handling async effects are defined in the `Initialize` variant.


  The difference from example #00 is now we have some data dynamically generated in the Parent.

  The integer `count` changes state periodically in this parent component. The new state is then sent as
  input to the Child component (slotted inside the Parent component).

  
  This example is quite simple. However, we can imagine more complex situation where we need to combine both
  the Input and Notify patterns. We can imagine some data is dynamically generated in the Parent, which
  is then sent as input to the Child. The Child, upon receiving this input, processes and notify the Parent
  with some output.
--}
