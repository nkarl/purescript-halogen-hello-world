module Next.Patterns.ParentQueries.Child.Button where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

-- This component can handle two action variants.
-- It can evaluate a `Click` action, or it can receive new input when its parent re-renders.
data Action
  = Click
  | Receive Input

-- This component accepts a label as input
type Input = { label :: String }

-- This component stores a label and an enabled flag in state
type State = { label :: String, enabled :: Boolean }

-- This component can notify parent components of one event, `Clicked`
data Output
  = Clicked

-- This component accepts two query variants.
--    2. a tell-style    query such that a Parent send    a `Boolean` value to   this Child.
--    1. a request-style query that that a Parent request a `Boolean` value from this Child.
data Query a
  = SetEnabled Boolean a
  | GetEnabled (Boolean -> a)
  

derive instance genericOutuput :: Generic Output _
instance showOuptut :: Show Output where
  show = genericShow

-- The Child will have the label "button" in the row where it's used, so we export a symbol proxy for convenience.
_label = Proxy :: Proxy "button"

component :: forall m. H.Component Query Input Output m
component = button

-- This component can accept type a `Query` and send a message of type `Output`.
-- This slot type is exported so that other components can use it when constructing their row of slots.
type Slot = H.Slot Query Output

-- The Parent supports one type of Child, which uses the `Slot` slot type.
-- You can have as many of this type of Child as there are integers.
-- Other types of Child components can be added as needed in this row, for example `dropdown :: Slot String`.
type Slots = ( button :: Slot Int )

-- This component
--    1. supports queries of type `Query`,
--    2. requires input of type `Input`, and
--    3. can send outputs of type `Output`.
-- It doesn't perform any effects, which we can tell because the `m` type parameter has no constraints.
button :: forall m. H.Component Query Input Output m
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

  -- This Child has no children. When the rendered button is clicked we evaluate the `Click` action.
  render :: State -> H.ComponentHTML Action () m
  render { label, enabled } =
    HH.button
      [ HE.onClick \_ -> Click ]
      [ HH.text $ label <> " (" <> (if enabled then "on" else "off") <> ")" ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of

    -- When we receive new input we update our `label` field in state.
    Receive input ->
      H.modify_ _ { label = input.label }

    -- When the button is clicked, we update the `enabled` field and output the `Clicked` event to Parent.
    Click -> do
      H.modify_ \state -> state { enabled = not state.enabled }
      H.raise Clicked

  handleQuery :: forall a. Query a -> H.HalogenM State Action () Output m (Maybe a)
  handleQuery = case _ of

    -- upon receiving a tell-style `SetEnabled` query with a boolean, we set the corresponding field.
    SetEnabled value next -> do
      H.modify_ _ { enabled = value }
      pure (Just next)

    -- upon receiving a request-style `GetEnabled` query, which is an action/callback that requires a boolean result,
    -- we get the boolean saved in state, and evaluate it and send the result as a reply.
    GetEnabled reply -> do
      enabled <- H.gets _.enabled
      pure (Just (reply enabled))
