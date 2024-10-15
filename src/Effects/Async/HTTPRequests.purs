module Effects.Async.HTTPRequests where

import Prelude

import Affjax.ResponseFormat as AXRF
import Affjax.Web as AX
import CSS (backgroundColor, border, borderRadius, padding, solid)
import CSS.Color as Color
import CSS.Size (em, px)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event

type State =
  { loading :: Boolean
  , username :: String
  , result :: Maybe String
  }

data Action
  = SetUsername String
  | MakeRequest Event

-- | Component with an initial state
component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }
  where
  initialState :: i -> State
  initialState _ =
    { loading: false
    , username: ""
    , result: Nothing
    }

-- the renderer function. creates a _form_ that accepts a state and produces an HTML
render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.form
    [ HE.onSubmit \e -> MakeRequest e ] -- props
    -- the form header
    [ HH.h1_ [ HH.text "Look up GitHub user" ]
    -- the label with input
    , HH.label_
        [ HH.div_ [ HH.text "Enter username:" ]
        , HH.input
            [ HP.value state.username
            , HE.onValueInput \s -> SetUsername s
            ]
        ]
    -- the submit button
    , HH.button
        [ HP.disabled state.loading -- NOTE: button should be disabled while loading content.
        --, HP.type_ HP.ButtonSubmit -- NOTE: default behavior is submit, no need to specify.
        ]
        [ HH.text "Fetch info" ]
    , HH.p_
        [ HH.text $ if state.loading then "Working..." else "" ]
    -- selects the render possiblities for the content of the request body
    , HH.div_ case state.result of
        Nothing -> []
        Just responseBody ->
          [ HH.h2_
              [ HH.text "Response:" ]
          , HH.pre
              [ CSS.style do
                  border solid (px 1.0) (Color.black)
                  borderRadius (em 0.5) (em 0.5) (em 0.5) (em 0.5)
                  padding (em 2.0) (em 2.0) (em 2.0) (em 2.0)
                  backgroundColor $ Color.hsl 26.0 0.93 0.78
              ]
              [ HH.code_ [ HH.text responseBody ] ]
          ]
    ] -- childs

-- event handler implemented for the _form_ component
-- accepts Action, which has 2 variants, either `SetUsername s` or `MakeRequest e`
handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of

  SetUsername username -> do
    H.modify_ _
      { username = username
      {- , result = Nothing -}
      -- BUG: screen should keep previous result until we click submit button.
      }

  MakeRequest e -> do
    H.liftEffect $ Event.preventDefault e -- lift the Effect into the HalogenM context
    username <- H.gets _.username
    H.modify_ _ { loading = true }
    response <- H.liftAff $ AX.get AXRF.string ("https://api.github.com/users/" <> username)
    H.modify_ _ { loading = false, result = map _.body (hush response) }

{--
  NOTE: In this example, we see that `HalogenM` is the outermost context layer.
    Inside this `HalogenM` context, we can run (lifted) contexts of the type `Effect` and `Aff`.

    In the the second case, `MakeRequest event`, we see the event-driven nature of the underlying
    Halogen/Node infrastructure. The runtime always listens for _any_ state changes and invokes
    the `render` function accordingly.
--}

{--
  NOTE: potential experimentation/improvement
  - combine SetUsername with MakeRequest
    - allows for retrieving user info in real time.
    - consideration: required local database index, in order to
      - prevent from being rate limited by GitHub API
--}
