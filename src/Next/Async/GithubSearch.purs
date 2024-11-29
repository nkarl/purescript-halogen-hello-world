module Next.Async.GithubSearch where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
    }
  where

  render _ =
    HH.div_
      [ HH.label
          []
          [ HH.text "Search"
          , HH.input
              [ HP.value "type to start searching..." ]
          ]
      ]
