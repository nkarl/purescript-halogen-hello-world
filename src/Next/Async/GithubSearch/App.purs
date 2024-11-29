module Next.Async.GithubSearch where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import MyUtils (className)
import Next.Async.GithubSearch.Component.ContentPanel as ContentPanel
import Next.Async.GithubSearch.Component.SearchBox as SearchBox

type State = { username :: Maybe String }

type Slots q o r = (component :: H.Slot q o Unit | r)

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
    }
  where
  initialState _ = { username: Nothing }

render :: forall s a q o r m. s -> HH.ComponentHTML a (Slots q o r) m
render _ =
  HH.div
    [ className "container-fluid d-flex flex-column p-5"
    ]
    -- search form
    [ HH.div
        [ className "row m-5" ]
        [ HH.div
            [ className "col" ]
            [ HH.slot_ SearchBox.label unit SearchBox.component unit ]
        ]
    -- result panel
    , HH.div
        [ className "row m-5" ]
        [ HH.div
            [ className "col" ]
            [ HH.slot_ ContentPanel.label unit ContentPanel.component unit ]
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
    1. User enter a key into the search box.
    2. User click 'Search'.
    3. The app searches.
    4. The app shows search result on the content panel.
      - content could be either valid or invalid, but must display both.

  3. Technical design:
    1. The Parent container tracks a state containing a posssible username (Maybe String).
    2. This state.username is produced as output by the Search box.
    3. The Parent send this state.username as input to the Display panel.

                                  State { username }
                                    :
                                    :
                                    :
                                Container
                                ^       |
                                |       |
      sends output string to    |       |  sends input string to
                      |---------|       |---------|
                      |                           |
                      |                           |
              onClick *                           v
                    SearchBox                   ContentPanel
                    ---------                   ------------
                    captures input              make async request
                                                show response result
--}

