module Next.Patterns.ParentQueries.Parent where

import Prelude

import Data.Array (fromFoldable)
import Data.Map.Internal (Map)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log, logShow)
import Halogen as H
import Halogen.HTML as HH
import Next.Patterns.ParentQueries.Child.Button as Button

-- The parent component supports one type of child component, which uses the
-- `Slot` slot type. You can have as many of this type of child component
-- as there are integers.
type Slots = ( button :: Button.Slot Int )

-- The parent component can only evaluate one action: handling output messages
-- from the button component, of type `Output`.
data Action = Handle Int Button.Output

-- The parent component maintains in local state the number of times all its
-- child component buttons have been clicked.
type State = { clicked :: Int, index :: Maybe Int }

-- The parent component uses no query, input, or output types of its own. It can
-- use any monad so long as that monad can run `Effect` functions.
component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
      -- The only internal event this component can handle are actions as
      -- defined in the `Action` type.
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: i -> State
  initialState _ = { clicked: 0, index: Nothing }

  -- We render three buttons, handling their output messages with the `Handle`
  -- action. When our state changes this render function will run again, each time
  -- sending new input (which contains a new label for the child button component
  -- to use.)
  render :: State -> H.ComponentHTML Action Slots m
  render { clicked } = do
    let clicks = show clicked
    HH.div_
      [ -- We render our first button with the slot id 0
        HH.slot Button._button 0 Button.child { label: clicks <> " Enabled" } $ Handle 0
        -- We render our second button with the slot id 1
      , HH.slot Button._button 1 Button.child { label: clicks <> " Power" } $ Handle 1
        -- We render our third button with the slot id 2
      , HH.slot Button._button 2 Button.child { label: clicks <> " Switch" } $ Handle 2
      ]

  handleAction :: Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of
    -- We handle one action, `Handle`, which itself handles the output messages
    -- of our button component.
    Handle idx output -> case output of
      -- There is only one output message, `Clicked`.
      Button.Clicked -> do
        -- When the `Clicked` message arises we will increment our clicked count
        -- in state, then send a query to the first button to tell it to be `true`,
        -- then send a query to all the child components requesting their current
        -- enabled state, which we log to the console.
        H.modify_ \state -> state { clicked = state.clicked + 1 }
        H.tell Button._button 0 (Button.SetEnabled true)
        on :: Map _ _ <- H.requestAll Button._button Button.GetEnabled
        logShow (on)
        let
            --arr = fromFoldable on
            on'' = on :: Map Int Boolean 
        log $ show (fromFoldable on'' :: Array _)
        --log $ show (arr)
        --log $ "at index: "  <> (show idx) <> " -> " <> show (index arr idx)
        --on' <- H.request Button._button idx Button.GetEnabled
        --log $ show (on')

--request
  --:: ∀ s a o m label slots q o' slot a _1
   --. Cons label (Slot q o' slot) _1 slots
  --=> IsSymbol label
  --=> Ord slot
  --=> Proxy label -> slot -> Request q a -> HalogenM state a slots o m (Maybe a)

--requestAll
  --:: ∀ s a o m label slots q o' slot a _1
   --. Cons label (Slot q o' slot) _1 slots
  --=> IsSymbol label
  --=> Ord slot
  --=> Proxy label -> Request q a -> HalogenM s a slots o m (Map slot a)
