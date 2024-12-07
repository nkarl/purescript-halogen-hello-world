module Next.Async.GithubSearcher where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (logShow)
import Halogen as H
import Halogen.HTML as HH
import MyUtils (className)
import Next.Async.GithubSearcher.Component.ContentPanel as ContentPanel
import Next.Async.GithubSearcher.Component.SearchBox as SearchBox

{--
  NOTE: This exercise is a practice in identifying, separating and monitoring data flow.

  It is a entirely possible to have only 2 components where the Container controls the display panel.
  In this case, the output from SearchBox is consumed immediately inside `render`.

  In this exercise, the output is "streamed" to a separate component "DisplayPanel". This serves 2 purposes:
    1. single responsibility: search only searches, display only displays.
    2. open design: we might want to intercept the content first, before letting it pass through to the display.
      - for example, filtering the content or augmenting it with some additional data
--}

type Content = Maybe String

type State = { content :: Content }

data Action = Handle SearchBox.Output

type Slots =
  ( searchBox :: SearchBox.Slot
  , contentPanel :: ContentPanel.Slot
  )

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
  initialState _ = { content: Just "" }

  handleAction :: Action -> H.HalogenM State Action (Slots) o m Unit
  handleAction = case _ of
    Handle output -> do
      H.modify_ _ { content = output }
      H.gets _.content >>= logShow

  render :: State -> HH.ComponentHTML Action (Slots) m
  render { content } =
    HH.div
      [ className "container-fluid d-flex flex-column p-5"
      ]
      -- search form
      [ HH.div
          [ className "row m-5" ]
          [ HH.div
              [ className "col" ]
              [ HH.slot SearchBox.label unit SearchBox.component unit $ Handle ]
          ]
      -- result panel
      , HH.div
          [ className "row m-5" ]
          [ HH.div
              [ className "col" ]
              [ HH.slot_ ContentPanel.label unit ContentPanel.component content ]
          ]
      ]

{--
  NOTE: interface description.

  This is a simple interface that is centered on the browser window, that has 2 parts:
    1. a search bar for the user to type in any combination of strings to search
    2. a square box under the search bar to display the result of the search in real time.

  NOTE: other requirements

  The search functionality must be able to request as the user type. Use the following
  guideline for development:
    1. implement search for complete keywords
      - only triggers a request when the user hits the Enter key or the button 'Search'.
    2. implement search on key strokes.
      - do this only after #1.

  NOTE: other considerations

    - Github should have a request cap (mabye 5000/day), so consider a timeout before each
      request.
--}

{--
  NOTE: Design

  1. Components: 3
    - a `div` Parent container
    - a search box component (Child)
    - a content panel component (Child)

  2. Desired behaviors:
    1. User enters a key into the search box.
    2. User clicks 'Search'/hits 'Enter' -> form is submitted.
    3. The app makes a request in the background.
    4. The app shows search result on the content panel.
      - content could be either valid or invalid, but must display both.

  3. Technical design:
    1. The Parent (container) tracks a state containing a posssible content (Maybe String).
    2. This state.content is the output from the Search Box.
    3. The Parent send this state.content as input to the Display Panel.

                                  State { content :: Maybe String }
                                    :
                                    :
      forms an input cycle          :
              |---------------* Container
              |                 ^       |
              |                 |       |
              |                 |       |                       
              |       |---------|       |---------| sends the output of SearchBox to the Display Panel
              |       |                           |
              v       |                           |
              onClick *                           v
                    SearchBox                   ContentPanel
                    ---------                   ------------
                    captures input              make async request
                    performs async-effect       show response result
--}

