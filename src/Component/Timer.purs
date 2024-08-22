module Component.Timer where

import Prelude
import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Subscription as HS

{--
  NOTE: there is 1 primary action
    - a numerical update on a clock `Tick`
  --}
data Action
  = Initialize
  | Tick

type State
  = Int

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }
  where
  initialState _ = 0

render :: forall m. State -> H.ComponentHTML Action () m
render seconds = HH.text ("You have been here for " <> show seconds <> " seconds.")

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    _ <- H.subscribe =<< timer Tick
    pure unit
  Tick -> H.modify_ \s -> s + 1

timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect $ HS.create
  _ <-
    H.liftAff $ Aff.forkAff
      $ forever do
          Aff.delay $ Milliseconds 1000.0
          H.liftEffect $ HS.notify listener val
  pure emitter
