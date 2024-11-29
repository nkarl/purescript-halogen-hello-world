module Next.Patterns.Query.Child.Button where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

type Input = String

type State = { label :: String, isEnabled :: Boolean }

data Action = Click

type Slot = forall q o a. H.Slot q o a

_label = Proxy :: Proxy "button"

button :: forall q o m. H.Component q Input o m
button = component

component :: forall q o m. H.Component q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }
  where

  initialState label = { label, isEnabled: false }

  render { label, isEnabled } =
    HH.button
      [ HE.onClick \_ -> Click ]
      [ HH.text $ label <> " (" <> (if isEnabled then " On" else " Off") <> ")"
      ]

  handleAction = case _ of
    Click -> do
      H.modify_ \s -> s { isEnabled = not s.isEnabled }
