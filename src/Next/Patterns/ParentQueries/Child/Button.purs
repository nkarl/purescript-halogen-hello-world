module Next.Patterns.ParentQueries.Child.Button where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

-- We now move on to the child component, a component called `button`.

-- This component can accept queries of type `Query` and send output
-- messages of type `Output`. This slot type is exported so that other
-- components can use it when constructing their row of slots.
type Slot = H.Slot Query Output

-- We think our button will have the label "button" in the row where it's used,
-- so we're exporting a symbol proxy for convenience.
_button = Proxy :: Proxy "button"

-- This component accepts two queries. The first is a request-style query that
-- lets a parent component request a `Boolean` value from us. The second is a
-- tell-style query that lets a parent component send a `Boolean` value to us.
data Query a
  = GetEnabled (Boolean -> a)
  | SetEnabled Boolean a

-- This component can notify parent components of one event, `Clicked`
data Output
  = Clicked

derive instance genericOutuput :: Generic Output _

instance showOuptut :: Show Output where
  show = genericShow

-- This component can handle two internal actions. It can evaluate a `Click`
-- action and it can receive new input when its parent re-renders.
data Action
  = Click
  | Receive Input

-- This component accepts a label as input
type Input = { label :: String }

-- This component stores a label and an enabled flag in state
type State = { label :: String, enabled :: Boolean }

-- This component supports queries of type `Query`, requires input of
-- type `Input`, and can send outputs of type `Output`. It doesn't
-- perform any effects, which we can tell because the `m` type parameter has
-- no constraints.
button :: forall m. MonadEffect m => H.Component Query Input Output m
button =
  H.mkComponent
    { initialState
    , render
      -- This component can handle internal actions, handle queries sent by a
      -- parent component, and update when it receives new input.
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< Receive
        }
    }
  where
  initialState :: Input -> State
  initialState { label } = { label, enabled: false }

  -- This component has no child components. When the rendered button is clicked
  -- we will evaluate the `Click` action.
  render :: State -> H.ComponentHTML Action () m
  render { label, enabled } =
    HH.button
      [ HE.onClick \_ -> Click ]
      [ HH.text $ label <> " (" <> (if enabled then "on" else "off") <> ")" ]

  handleAction
    :: Action
    -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    -- When we receive new input we update our `label` field in state.
    Receive input ->
      H.modify_ _ { label = input.label }

    -- When the button is clicked we update our `enabled` field in state, and
    -- we notify our parent component that the `Clicked` event happened.
    Click -> do
      H.modify_ \state -> state { enabled = not state.enabled }
      H.raise Clicked

  handleQuery
    :: forall a
     . Query a
    -> H.HalogenM State Action () Output m (Maybe a)
  handleQuery = case _ of
    -- When we receive a the tell-style `SetEnabled` query with a boolean, we
    -- set that value in state.
    SetEnabled value next -> do
      H.modify_ _ { enabled = value }
      --log $ show next
      pure (Just next)

    -- When we receive a the request-style `GetEnabled` query, which requires
    -- a boolean result, we get a boolean from our state and reply with it.
    GetEnabled reply -> do
      enabled <- H.gets _.enabled
      pure (Just (reply enabled))
