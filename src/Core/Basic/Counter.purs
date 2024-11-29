module Core.Basic.Counter where

import Prelude

import Halogen (Component)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Input = Unit

type State = Int

data Action = Decrement | Increment

component :: forall q i o m. Component q i o m
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

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render s =
    HH.div_
      [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
      , HH.text (show s)
      , HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "+" ]
      ]

  handleAction ∷ Action -> H.HalogenM State Action () o m Unit
  handleAction = case _ of
    Decrement -> H.modify_ \s -> s - 1
    Increment -> H.modify_ \s -> s + 1
