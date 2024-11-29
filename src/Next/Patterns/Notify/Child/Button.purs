module Next.Patterns.Notify.Child.Button where

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

instance showOutput :: Show Output where
  show = genericShow

data Action = Click

type Slots = ( component :: forall q. H.Slot q Output Int )

component :: forall q i m. MonadEffect m => H.Component q i Output m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        }
    }
  where

  render :: forall s w. s -> HH.HTML w Action
  render _ =
    HH.button
      [ HE.onClick \_ -> Click ]
      [ HH.text "Click Me!" ]

  handleAction :: forall s. Action -> H.HalogenM s Action () Output m Unit
  handleAction = case _ of
                     Click ->
                       do log ("component is " <> show Clicked)
                          H.raise Clicked

{--
  The Child does not track any local state.
  
  However, it generates Action events. These events are sent as output to Parent.

  This pattern is suited for we have many action variants on a UI component.

  This is a simple example. However, we can imagine more complex situations where
  we need to handle a collection of actions done on a Child component. These
  actions could be organized in a record type and sent as notification/output
  to the Parent node.

  Furthermore, these actions might be async, which require the MonadAff constraint.
--}
