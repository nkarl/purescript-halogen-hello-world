module Next.Patterns.Query.OldExample where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

type HTML a slots m = H.ComponentHTML a slots m

{--
  parent component

  it has 3 buttons.

  Button 1: on click, increase the click counter. Always set to `enabled='On'`.
  Button 2: on click, increase the click counter and set to opposite of `enabled`.
  Button 3: on click, increase the click counter and set to opposite of `enabled`.

  The click counter is control by this component (Parent). Only the on/off switch (action/query) is
  communicated to the buttons (Child).
--}

type State =
  { clicked :: Int
  , whichButton :: Maybe Int
  }

data Action = Handle Int ButtonOutput

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }

  where
  initialState _ = { clicked: 0, whichButton: Nothing }

  render :: State -> HTML Action Slots m
  render { clicked } =
    let
      clicks = show clicked
    in
      HH.div_
        [ HH.slot _button 0 button { label: clicks <> " Enabled" } $ Handle 0
        , HH.slot _button 1 button { label: clicks <> " Power" } $ Handle 1
        , HH.slot _button 2 button { label: clicks <> " Switch" } $ Handle 2
        ]

  handleAction = case _ of
    Handle _ out -> case out of
      ButtonClicked -> do
        H.modify_ \s -> s { clicked = s.clicked + 1 }
        H.tell _button 0 (SetEnabled true) >>= logShow -- always stays On
        H.requestAll _button GetEnabled >>= logShow

type Slots = (button :: Slot Int)

{--
  child component
--}

type Slot = H.Slot ButtonQuery ButtonOutput

_button = Proxy :: Proxy "button"

type ButtonInput = { label :: String }

type ButtonState = { label :: String, enabled :: Boolean }

data ButtonAction
  = Click
  | Receive ButtonInput

data ButtonQuery a
  = SetEnabled Boolean a
  | GetEnabled (Boolean -> a)

data ButtonOutput = ButtonClicked

button :: forall m. H.Component ButtonQuery ButtonInput ButtonOutput m
button =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< Receive
        }
    }

  where
  --initialState :: ButtonInput -> ButtonState
  initialState { label } = { label, enabled: false }

  --render :: ButtonState -> HTML ButtonAction () m
  render { label, enabled } =
    HH.button
      [ HE.onClick \_ -> Click ]
      [ HH.text $ label <> " (" <> (if enabled then " On" else " Off") <> ")" ]

  handleAction = case _ of
    Click -> do
      H.modify_ \s -> s { enabled = not s.enabled }
      H.raise ButtonClicked

    Receive input -> do
      H.modify_ _ { label = input.label }

  handleQuery :: forall a. ButtonQuery a -> H.HalogenM ButtonState ButtonAction () ButtonOutput m (Maybe a)
  handleQuery = case _ of
    SetEnabled isEnabled next -> do
      H.modify_ _ { enabled = isEnabled }
      pure $ Just next

    GetEnabled reply -> do
      { enabled } <- H.get
      pure $ Just (reply enabled)
