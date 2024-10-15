module Next.Patterns.ChildNotifies.Example where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS
import Type.Proxy (Proxy(..))

type Slots :: Row Type
type Slots = ( button :: forall q. H.Slot q Output Int )

type State = { count :: Int }

data Action
  = Initialize
  | Increment
  | Handle Output


-- parent component

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handle
        , initialize = Just Initialize
        }
    }
  where
  initialState :: i -> State
  initialState _ = { count: 0 }

  _button = Proxy :: Proxy "button"

  render :: State -> H.ComponentHTML Action Slots m
  --render _ = HH.div_ [ button { label: "Click me" } ]
  render { count } = HH.div_
    [ HH.label_
        [ HH.text "Some random label: "
        , HH.slot _button 0 button { label: "Click me " <> show count } Handle -- FIX: fixed the reset here.
        ]
    ]

  -- handle action
  handle :: Action -> H.HalogenM State Action Slots o m Unit
  handle action = case action of
    Initialize -> do
      { emitter, listener } <- H.liftEffect $ HS.create
      void $ H.subscribe emitter
      void $ H.liftAff $ Aff.forkAff $ forever do
        Aff.delay $ Milliseconds 1000.0
        H.liftEffect $ HS.notify listener Increment

    Increment -> H.modify_ \s -> s { count = s.count + 1 }

    Handle Clicked -> H.modify_ \s -> s { count = 0 } -- BUG: doesn't reset because we need `HH.slot` for the output. `HH.slot_` discard the output.

-- child component

type Input = { label :: String }
type State' = { label :: String }
data Action' = Receive Input | Click
data Output = Clicked

button :: forall q {-i-} m. H.Component q Input Output m
button =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = childHandle
        , receive = Just <<< Receive
        }
    }
  where
  initialState :: Input -> State'
  initialState { label } = { label }

  render :: State' -> H.ComponentHTML Action' () m
  render { label } = HH.button [ HE.onClick \_ -> Click ] [ HH.text label ]

  childHandle :: Action' -> H.HalogenM State' Action' () Output m Unit
  childHandle = case _ of
    Receive input -> H.modify_ _ { label = input.label }
    Click -> H.raise Clicked
