module Next.Patterns.ParentQueries.Parent where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import Halogen as H
import Halogen.HTML as HH
import Next.Patterns.ParentQueries.Child.Button as Child.Button

-- | The Parent evaluates only one action: handling events output from the Child button (type `Output`).
data Action = Handle Int Child.Button.Output

-- | The Parent maintains a local state for the number of clicks on all its Child buttons.
type State =
  { clicked :: Int
  , index   :: Maybe Int -- for indexing the specific Child button
  }

-- | The Parent uses no q, i, o. It uses any monad that is constrained to `Effect`ful functions.
component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval -- Only one action event is handled here, as defined in the `Action` type.
        { handleAction = handleAction
        }
    }
  where
  initialState :: i -> State
  initialState _ = { clicked: 0, index: Nothing }

  -- We render three buttons, handling their output messages with the `Handle` action.
  -- When this component's state changes, `render` will run again, each time sending new input
  -- (which contains a new label for the Child button to use.)
  render :: State -> H.ComponentHTML Action (Child.Button.Slots) m
  render { clicked } = do
    let clicks = show clicked
    HH.div_
      [ HH.slot Child.Button._label 0 Child.Button.component { label: clicks <> " Enabled" } $ Handle 0
      , HH.slot Child.Button._label 1 Child.Button.component { label: clicks <> " Power"   } $ Handle 1
      , HH.slot Child.Button._label 2 Child.Button.component { label: clicks <> " Switch"  } $ Handle 2
      ]

  handleAction :: Action -> H.HalogenM State Action (Child.Button.Slots) o m Unit
  handleAction = case _ of
    -- The only action variant, `Handle *`, take cares of the output messages from our button component.
    -- the `Handle` function does not use the Child button's index. It is only used by the Parent.
    Handle _ output -> case output of

      Child.Button.Clicked -> do -- There is only one output variant, `Clicked`.

        -- When the `Clicked` message arises we will
        --    1. increment our `state.clicked` count
        --    2. send a query to the first button to tell it to be `true`
        --    3. send a query to all child buttons to return their state (grouped and logged to the console)
        H.modify_     \state -> state { clicked = state.clicked + 1 }
        H.tell        Child.Button._label 0 (Child.Button.SetEnabled true) >>= logShow
        H.requestAll  Child.Button._label Child.Button.GetEnabled >>= logShow
