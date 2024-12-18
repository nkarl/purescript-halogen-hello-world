module Core.Pattern.Notify.Parent where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

import Core.Pattern.Notify.Child.Button as Child.Button

type State = Int

data Action = HandleButton Child.Button.Output

derive instance genericAction :: Generic Action _

instance showAction :: Show Action where
  show = genericShow

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        }
    }
  where

  initialState :: i -> State
  initialState _ = 0

  render :: State -> H.ComponentHTML Action Child.Button.Slots m
  render s =
    HH.div_
      -- alternately, since there is only one Child, we can use `HH.slot_` and `unit`.
      [ HH.slot (Proxy :: Proxy "component") 0 Child.Button.component s HandleButton
      , HH.div_ [ HH.text (show s) ]
      ]

  handleAction :: Action -> H.HalogenM State Action Child.Button.Slots o m Unit
  handleAction = case _ of
                     HandleButton output -> case output of
                       Child.Button.Clicked ->
                          do H.modify_ \s -> s + 1
                             H.get >>= logShow -- gets and shows the current state in the browser console.


{--
  This example shows how to implement the Notify pattern: Child notifies Parent.
  
  In this example, the Parent tracks an integer state. The Parent component is notified whenever
  an action event is fired/output from the Child. There is only one action variant in the Child,
  hence only one variant case is handled by the Parent. Upon receiving this action, the Parent's
  state is updated. This triggers the Parent to rerender.

  NOTE: Not entire sure about the use cases for a MonadAff Parent. When do we need to use MonadAff?

    - Possibly when we need to wait for output from multiple childs asynchronously.
    - This pattern seems to have a larger complexity space bc of 2 cases:

      1. async action events from a single Child.
      2. async action events from multiple async Child nodes.

  This is a simple example. However, we can imagine more complex situations. A possible scenario is
  when the Parent's state is in turn used as input for the Child. This input might be used to constrain
  the kinds of actions generated by the Child.
--}
