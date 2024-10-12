module Next.Patterns.ParentInputsChild where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Subscription as HS
import Type.Proxy (Proxy(..))

import CSS (backgroundColor, border, borderRadius, padding, solid)
import CSS.Color as Color
import CSS.Size (em, px)
--import Halogen.HTML.CSS as CSS

import Halogen.HTML.CSS as HCSS

{-
  NOTE: PARENT LOGIC
-}

type Slots        = (child :: forall q. H.Slot q Void Unit)

type ParentState  = { count :: Int }

data ParentAction
  = Initialize
  | Increment

-- PARENT

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where
  initialState :: i -> ParentState
  initialState _ = { count: 0 }

  render :: ParentState -> H.ComponentHTML ParentAction Slots m
  render { count } =
    HH.div_ [ HH.slot_ _child unit child { label: show count } ]

  handleAction :: ParentAction -> H.HalogenM ParentState ParentAction Slots o m Unit
  handleAction = case _ of
    Initialize -> do
      { emitter, listener } <- H.liftEffect HS.create
      void $ H.subscribe emitter
      void
        $ H.liftAff
        $ Aff.forkAff
        $ forever do
            Aff.delay $ Milliseconds 1000.0
            H.liftEffect $ HS.notify listener Increment
    Increment -> H.modify_ \st -> st { count = st.count + 1 }

{-
  NOTE: CHILD LOGIC
-}

_child = Proxy :: Proxy "child"

-- Now we turn to our child component, the child.
type InputFromParent  = { label :: String }

type ChildState       = { label :: String }

data ChildAction      = Receive InputFromParent

-- CHILD

child :: forall q o m. H.Component q InputFromParent o m
child =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
  where
  initialState :: InputFromParent -> ChildState
  initialState { label } = { label }

  render :: forall action. ChildState -> H.ComponentHTML action () m
  render { label } = HH.button
    [ HCSS.style do
        border solid (px 1.0) (Color.black)
        borderRadius (em 0.5) (em 0.5) (em 0.5) (em 0.5)
        padding (em 2.0) (em 2.0) (em 2.0) (em 2.0)
        backgroundColor $ Color.hsl 26.0 0.93 0.78

    ]
    [ HH.text label ]

  handleAction :: ChildAction -> H.HalogenM ChildState ChildAction () o m Unit
  handleAction = case _ of
    Receive input -> H.modify_ _ { label = input.label }
