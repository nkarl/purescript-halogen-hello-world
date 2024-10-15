module Next.Patterns.ParentInputs.Child01 where

import Prelude

import Data.Maybe (Maybe(..))
import Control.Monad.Rec.Class (forever)

import Halogen                                                      as H
import Halogen.HTML                                                 as HH
import Halogen.HTML.CSS                                             as HCSS
import Halogen.Subscription                                         as HS

import Effect.Aff                                                   as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Aff (Milliseconds(..))

import CSS (backgroundColor, border, borderRadius, padding, solid)
import CSS.Color                                                    as Color
import CSS.Size (em, px)

import Type.Proxy (Proxy(..))


---- PARENT --------------------------------------------------------------------------

type Slots = (child :: forall q. H.Slot q Void Unit)

type State = { count :: Int }

data Action
  = Initialize
  | Increment

-- PARENT COMPONENT

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
  initialState :: i -> State
  initialState _ = { count: 0 }

  --render :: State -> HH.HTML (H.ComponentSlot Slots m Action) Action
  render :: State -> H.ComponentHTML Action Slots m
  render { count } =
    HH.div_ [ HH.slot_ _ChildComponent unit child { label: show count } ]

  handleAction :: Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of

                     Initialize -> do
                       { emitter, listener } <- H.liftEffect HS.create
                       void
                         $ H.subscribe emitter
                       void
                         $ H.liftAff
                         $ Aff.forkAff
                         $ forever do
                             Aff.delay $ Milliseconds 1000.0
                             H.liftEffect $ HS.notify listener Increment
                      
                     Increment -> H.modify_ \state -> state { count = state.count + 1 }


---- CHILD --------------------------------------------------------------------------

_ChildComponent = Proxy :: Proxy "child"

-- Now we turn to our child component, the child.
type InputFromParent = { label :: String }

type State'  = { label :: String }

data Action' = Receive InputFromParent

-- CHILD COMPONENT

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
  initialState :: InputFromParent -> State'
  initialState { label } = { label }

  render :: forall action. State' -> H.ComponentHTML action () m
  render { label } =
    HH.button
      [ HCSS.style do
          border solid (px 1.0) (Color.black)
          borderRadius (em 0.5) (em 0.5) (em 0.5) (em 0.5)
          padding (em 2.0) (em 2.0) (em 2.0) (em 2.0)
          backgroundColor $ Color.hsl 26.0 0.93 0.78
      ]
      [ HH.text label ]

  handleAction :: Action' -> H.HalogenM State' Action' () o m Unit
  handleAction = case _ of

                     Receive input -> H.modify_ _ { label = input.label }
