module Next.Async.GithubSearch.Component.SearchBox where

import Prelude

import Data.String as String
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

import MyUtils (className)

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
  | SearchButtonClicked

type Output = UserName

label = Proxy :: Proxy "searchBox"

searchBox :: forall q i m. H.Component q i Output m
searchBox = component

component :: forall q i m. H.Component q i Output m
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

  render :: forall s. s -> H.ComponentHTML Action () m
  render _ =
    HH.div [ className "input-group w-50 container-fluid" ]
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
          , HE.onClick \_ -> SearchButtonClicked
          ]
          [ HH.text "Search" ]
      ]

  handleAction = case _ of
    Capture input -> do
      let
        isValidLength = String.length input < 3

      unless isValidLength do -- NOTE: parse, don't validate.
        H.modify_ \_ -> input

    SearchButtonClicked -> do
      username <- H.get
      H.raise username
