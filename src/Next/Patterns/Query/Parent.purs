module Next.Patterns.Query.Parent where

import Prelude

import Halogen as H
import Halogen.HTML as HH

import Next.Patterns.Query.Child.Button as Button

type State = { clickCount :: Int }

type WhichButton = Int

data Action = Handle WhichButton Button.Output

type Slots = ( button :: Button.Slot )

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }
  where

  initialState _ = { clickCount: 0 }

  render { clickCount } =
    let
      clicks = show clickCount
    in
      HH.div_
        [ HH.slot Button._label 0 Button.component (clicks <> " Enabled") $ Handle 0
        , HH.slot Button._label 1 Button.component (clicks <> " Power  ") $ Handle 1
        , HH.slot Button._label 2 Button.component (clicks <> " Switch ") $ Handle 2
        ]

  handleAction = case _ of
    Handle _ output -> case output of
      Button.Clicked -> do
        H.modify_ \s -> s { clickCount = s.clickCount + 1 }
