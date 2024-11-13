module Next.Patterns.ParentInputs.Parent01 where

import Prelude

import Data.Maybe (Maybe(..))
import Control.Monad.Rec.Class (forever)

import Halogen                                                      as H
import Halogen.HTML                                                 as HH
import Halogen.Subscription                                         as HS

import Effect.Aff                                                   as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Aff (Milliseconds(..))

import Type.Proxy (Proxy(..))
import Next.Patterns.ParentInputs.Child01 as Child

type State = { count :: Int }

data Action = Initialize | Increment

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
  render :: State -> H.ComponentHTML Action Child.Slots m
  render { count } =
    HH.div_ [ HH.slot_ (Proxy :: Proxy "component") unit Child.component { label: show count } ]

  handleAction :: Action -> H.HalogenM State Action Child.Slots o m Unit
  handleAction = case _ of

                     Initialize ->
                       do { emitter, listener } <- H.liftEffect HS.create
                          void $ H.subscribe emitter
                          void $ H.liftAff $ Aff.forkAff $ forever
                            do Aff.delay $ Milliseconds 1000.0
                               H.liftEffect $ HS.notify listener Increment
                      
                     Increment ->
                       H.modify_ \state -> state { count = state.count + 1 }
