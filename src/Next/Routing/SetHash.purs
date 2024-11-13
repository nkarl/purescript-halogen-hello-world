module Next.Routing.SetHash where

import Prelude

import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Routing.Hash (setHash)


type State  = String

data Action = SetHash String

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
  { initialState: initialState
  , render
  , eval: H.mkEval H.defaultEval
    { handleAction = handleAction
    }
  }

  where
    initialState _ = ""

    render :: State -> H.ComponentHTML Action () m
    render _ = do
      let
          s = "hello-world"
      HH.div_
        [ HH.button
        [ HE.onClick \_ -> SetHash s ]
          [ HH.text (show s) ]
        ]

    handleAction = case _ of
      SetHash s -> do
        H.liftEffect $ setHash ("/" <> s)
        H.modify_ \_ -> s
