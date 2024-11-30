module Core.Effect.Async.HTTPRequestsNew where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import MyUtils (className)

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
      [ className "container-fluid d-flex flex-column p-5" ]
      [ HH.div
          [ className "row mx-5" ]
          [ HH.div
              [ className "col" ]
              [ HH.h1
                  [ className "input-grouop w-50 container-fluid" ]
                  [ HH.text "Look up GitHub User" ]
              ]
          ]
      , HH.div
          [ className "row m-5" ]
          [ HH.div
              [ className "col" ]
              [ HH.div
                  [ className "input-group w-50 container-fluid" ]
                  [ HH.span
                      [ className "input-group-text" ]
                      [ HH.text "username" ]
                  , HH.input
                      [ className "form-control"
                      , HP.placeholder "type something to search..."
                      ]
                  , HH.button
                      [ className "btn btn-primary"
                      ]
                      [ HH.text "Search" ]
                  ]
              ]
          ]
      , HH.div
          [ className "row m-5" ]
          [ HH.div
              [ className "col" ]
              [ HH.div_
                []
              ]
          ]
      ]
