module Next.Async.GithubSearcher.Component.SearchBox where

import Prelude

import Affjax.ResponseFormat as AXRF
import Affjax.Web as AXW
import Data.Either (hush)
import Data.Maybe (Maybe)
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import MyUtils (className)
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.Event.Event as Event

{--
  NOTE: description.

  This component captures user input locally. It tracks the captured input in a
  state, with the condition that the input string must be longer than 3 characters.

  A small note about "Parse, Don't Validate". We simply capture any input that pass
  the condition and ignore the rest.

  - [x] capture input with length >= 3
  - [ ] check input for invalid characters (not alpha-numeric).
  - [x] send capture user input to the Parent container.
--}

type Slot = forall q. H.Slot q Output Unit

type UserName = String

type State = UserName

data Action
  = Capture UserName
  | SearchButtonClicked Event

type Output = Maybe String

data Query a = GetUserData (UserName -> a)

label = Proxy :: Proxy "searchBox"

searchBox :: forall q i m. MonadAff m => H.Component q i Output m
searchBox = component

component :: forall q i m. MonadAff m => H.Component q i Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }
  where
  initialState _ = ""

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Capture input -> do
      let
        isValidLength = String.length input < 3

      unless isValidLength do
        H.modify_ \_ -> input

    SearchButtonClicked e -> do
      H.liftEffect $ Event.preventDefault e
      username <- H.get
      let baseGitHubApi = "https://api.github.com/users/"
      response <- H.liftAff $ AXW.get AXRF.string $ baseGitHubApi <> username
      H.raise $ _.body <$> (hush response)

  render :: forall s. s -> H.ComponentHTML Action () m
  render _ =
    HH.form
      [ className "input-group w-50 container-fluid"
      , HE.onSubmit \e -> SearchButtonClicked e
      ]
      [ HH.span
          [ className "input-group-text" ]
          [ HH.text "username"
          ]
      , HH.input
          [ className "form-control"
          , HP.placeholder "type to start searching..."
          , HE.onValueInput \s -> Capture s
          ]
      , HH.button
          [ className "btn btn-primary"
          , HP.disabled false
          ]
          [ HH.text "Search" ]
      ]

