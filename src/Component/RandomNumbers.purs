module Component.RandomNumbers where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State
  = Maybe Number

data Action
  = Generate
  | Initialize
  | Finalize

foreign import randomImpl :: Effect Number

random :: Effect Number
random = randomImpl

component ::
  forall query input output m.
  MonadEffect m =>
  H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              , finalize = Just Finalize
              }
    }

initialState :: forall input. input -> State
initialState _ = Nothing

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  let
    value = maybe "No number generated yet" show state
  HH.div_
    [ HH.h1_ [ HH.text "Random number" ]
    , HH.p_ [ HH.text ("Current value: " <> value) ]
    , HH.button
        [ HE.onClick \_ -> Generate ]
        [ HH.text "Generate new number" ]
    ]

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    handleAction Generate
    number <- H.get
    log ("Initialize: " <> show number)
  Generate -> do
    handleAction Finalize
    newNumber <- H.liftEffect random
    H.put (Just newNumber)
  Finalize -> do
    number <- H.get
    log ("Finalized! Last number was: " <> show number)
