module Next.Patterns.Query.Parent where

import Prelude

import Halogen as H
import Halogen.HTML as HH

import Next.Patterns.Query.Child.Button as Button

type State = { clickCount :: Int }

type Slots = ( button :: Button.Slot )

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
    }
  where

  initialState _ = { clickCount: 0 }

  render { clickCount } =
    let
      clicks = show clickCount
    in
      HH.div_
        [ HH.slot_ Button._label 0 Button.component (clicks <> " Enabled")
        , HH.slot_ Button._label 1 Button.component (clicks <> " Enabled")
        , HH.slot_ Button._label 2 Button.component (clicks <> " Enabled")
        ]
