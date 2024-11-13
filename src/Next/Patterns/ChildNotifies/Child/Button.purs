module Next.Patterns.ChildNotifies.Child.Button where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Output = Clicked

derive instance genericOutput :: Generic Output _

type Slots = ( component :: forall q. H.Slot q Output Int )

instance showOutput :: Show Output where
  show = genericShow

data Action = Click

component :: ∀ q i m. MonadEffect m => H.Component q i Output m
component =
  H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
    { handleAction = handleAction
    }
  }
  where

  render :: forall t w. t -> HH.HTML w Action
  render _ =
    HH.button
      [ HE.onClick \_ -> Click ]
      [ HH.text "Not Clicked. Click Me!" ]

  handleAction :: forall s. Action -> H.HalogenM s Action () Output m Unit
  handleAction = case _ of
                     Click ->
                       do H.liftEffect $ log $ "component is " <> show Clicked
                          H.raise Clicked

{--
  The Child does not track any local state.
  
  However, it generates Action events. Best suited for when we need to define
  action variants on the UI.

  This is a simple example. However, we can imagine more complex situations where
  we need to handle a collection of actions done on a Child component. These
  actions could be organized in a record type and sent as notification/output
  to the Parent node.

  Furthermore, these actions might be async, which require the MonadAff
  constraint.
--}
