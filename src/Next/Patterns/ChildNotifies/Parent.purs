module Next.Patterns.ChildNotifies.Parent where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Next.Patterns.ChildNotifies.Parent.Button as B

type Slots = ( button :: forall q. H.Slot q B.Output Int )

type State = Int

data Action = HandleButton B.Output

derive instance genericAction :: Generic Action _

instance showAction :: Show Action where
  show = genericShow

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
    { handleAction = handleAction
    }
  }

  where
  initialState :: i -> State
  initialState _ = 0

  render :: State -> H.ComponentHTML Action Slots m
  render s =
    HH.div_
      [ HH.slot B._button 0 B.button s HandleButton
      , HH.div_ [ HH.text (show s) ]
      ]

  handleAction :: Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of
                     HandleButton output -> case output of
                       B.Clicked -> do
                          H.modify_ \s -> s + 1
                          s <- H.get
                          H.liftEffect $ log $ show s
