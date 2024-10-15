module Next.Patterns.ChildNotifies.Parent.Button where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

_button = Proxy :: Proxy "button"

data Output = Clicked

derive instance genericOutput :: Generic Output _

instance showOutput :: Show Output where
  show = genericShow

data Action = Click

button :: ∀ q i m. MonadEffect m => H.Component q i Output m
button =
  H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
    { handleAction = handleAction
    }
  }

  where

  render _ =
    HH.button
      [ HE.onClick \_ -> Click ]
      [ HH.text "Click Me" ]

  handleAction :: forall s. Action -> H.HalogenM s Action () Output m Unit
  handleAction = case _ of
                     Click -> do
                        H.liftEffect $ log $ "button is " <> show Clicked
                        H.raise Clicked

