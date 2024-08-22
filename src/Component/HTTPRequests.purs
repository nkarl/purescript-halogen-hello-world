module Component.HTTPRequests where

import Prelude

import Affjax.ResponseFormat as AXRF
import Affjax.Web as AX
import CSS (backgroundColor, border, borderRadius, color, margin, padding, solid)
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

type State
  = { loading :: Boolean
    , username :: String
    , result :: Maybe String
    }

data Action
  = SetUsername String
  | MakeRequest Event

-- | Component with an initial state
component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              }
    }
  where
  initialState :: forall i. i -> State
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
        [ HP.disabled state.loading
        , HP.type_ HP.ButtonSubmit
        ]
        [ HH.text "Fetch info" ]
    , HH.p_
        [ HH.text $ if state.loading then "Working..." else "" ]
    -- selects the render possiblities for the content of the request body
    , HH.div_ case state.result of
        Nothing -> []
        Just res ->
          [ HH.h2_
              [ HH.text "Response:" ]
          , HH.pre
              [ CSS.style do
                  border solid (px 1.0) (Color.black)
                  borderRadius (em 0.5) (em 0.5) (em 0.5) (em 0.5)
                  padding (em 2.0) (em 2.0) (em 2.0) (em 2.0)
                  backgroundColor $ Color.hsl 26.0 0.93 0.78
              ]
              [ HH.code_ [ HH.text res ] ]
          ]
    ] -- childs

-- event handler implemented for the _form_ component
-- accepts two possibilities, either `SetUsername s` or `MakeRequest e`
handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  SetUsername username -> do
    H.modify_ _ { username = username {-, result = Nothing -} } -- FIX: do not update result until we set the new result below.
  MakeRequest event -> do
    H.liftEffect $ Event.preventDefault event
    username <- H.gets _.username
    H.modify_ _ { loading = true }
    response <- H.liftAff $ AX.get AXRF.string ("https://api.github.com/users/" <> username)
    H.modify_ _ { loading = false, result = map _.body (hush response) }

{--
  NOTE: In this example, we see that `HalogenM` is the outermost context.
    Inside this context, we can run (lifted) `Effect` and `Aff`.
--}
