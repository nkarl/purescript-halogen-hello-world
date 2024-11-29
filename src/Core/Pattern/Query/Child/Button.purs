module Core.Pattern.Query.Child.Button where

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

data Query a
  = SetSwitch Boolean a
  | GetSwitch (Boolean -> a)

type Slot = H.Slot Query Output

_label = Proxy :: Proxy "button"

button :: forall m. H.Component Query Input Output m
button = component

component :: forall m. H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        , handleQuery = handleQuery
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

  handleQuery :: forall a. Query a -> H.HalogenM State Action () Output m (Maybe a)
  handleQuery = case _ of
    SetSwitch enabled any -> do
       H.modify_ \s -> s { isEnabled = enabled }
       pure $ Just any

    GetSwitch reply -> do
       { isEnabled } <- H.get
       pure $ Just (reply isEnabled)
