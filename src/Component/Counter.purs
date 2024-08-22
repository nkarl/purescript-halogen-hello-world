module Component.Counter where

import Prelude
import Halogen (Component)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State
  = Int

type Slots :: forall k. Row k
type Slots
  = ()

data Action
  = Increment
  | Decrement

component :: forall output m query input. Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: input -> State
  initialState _ = 0

  render state =
    HH.div_
      [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
      , HH.div_ [ HH.text $ show state ]
      , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
      ]

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Increment -> H.modify_ \state -> state + 1
    Decrement -> H.modify_ \state -> state - 1
