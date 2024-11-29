module Next.Patterns.Query.Child.Button where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

type Input = String

type State = { label :: String, isEnabled :: Boolean }

data Action
  = Click
  | Receive Input

data Output = Clicked

type Slot = forall q o a. H.Slot q o a

_label = Proxy :: Proxy "button"

button :: forall q m. H.Component q Input Output m
button = component

component :: forall q m. H.Component q Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
  where

  initialState label = { label, isEnabled: false }

  render { label, isEnabled } =
    HH.button
      [ HE.onClick \_ -> Click ]
      [ HH.text $ label <> (if isEnabled then " On" else " Off")
      ]

  handleAction = case _ of
    Click -> do
      H.modify_ \s -> s { isEnabled = not s.isEnabled }
      H.raise Clicked

    Receive input -> do
      H.modify_ \s -> s { label = input }
