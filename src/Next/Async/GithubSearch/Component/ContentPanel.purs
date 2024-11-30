module Next.Async.GithubSearch.Component.ContentPanel where

import Prelude

import Affjax.ResponseFormat as AXRF
import Affjax.Web as AX
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import MyUtils (className)
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.Event.Event as Event

type UserName = String

type Input = UserName

type State =
  { loading :: Boolean
  , username :: UserName
  , result :: Maybe String
  }

data Action = MakeRequest Event

type Slot = forall q o. H.Slot q o Unit

label = Proxy :: Proxy "contentPanel"

contentPanel :: forall q o m. MonadAff m => H.Component q Input o m
contentPanel = component

component :: forall q o m. MonadAff m => H.Component q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }
  where
  initialState username =
    { loading: false
    , username: username
    , result: Nothing
    }

  --render :: State -> H.ComponentHTML Action () m
  render _ =
    HH.div
      [ className "border border-dark-subtle p-3 rounded h-50" ]
      [ HH.text "placeholder JSON" ]

  handleAction :: Action -> H.HalogenM State Action () o m Unit
  handleAction = case _ of
    MakeRequest e -> do
      H.liftEffect $ Event.preventDefault e
      --user <- H.liftEffect $ H.gets _.username
      --H.modify_ _ { loading = true }
      --response <- AX.get AXRF.string ("https://api.github.com/users/" <> user)
      --H.modify_ _ { loading = false, result = map _.body (hush response) }
      pure unit

