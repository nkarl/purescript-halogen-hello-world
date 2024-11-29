module Next.Async.GithubSearch where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import MyUtils (className)

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

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
    }
  where

  render _ =
    HH.div
      [ className "container-fluid d-flex flex-column p-5"
      ]
      -- search form
      [ HH.div
          [ className "row m-5" ]
          [ HH.div
              [ className "col" ]
              [ HH.div [ className "input-group" ]
                  [ HH.span
                      [ className "input-group-text" ]
                      [ HH.text "username:"
                      ]
                  , HH.input
                      [ className "form-control"
                      , HP.placeholder "type to start searching..."
                      ]
                  , HH.button
                      [ className "btn btn-primary"
                      , HP.disabled false
                      ]
                      [ HH.text "Search" ]
                  ]
              ]
          ]
      -- result panel
      , HH.div
          [ className "row m-5 border border-dark-subtle p-3 rounded" ]
          [ HH.div
              [ className "" ]
              [ HH.text "placeholder JSON"
              ]
          ]
      ]
