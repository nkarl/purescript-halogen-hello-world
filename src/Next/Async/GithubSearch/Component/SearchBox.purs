module Next.Async.GithubSearch.Component.SearchBox where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import MyUtils (className)
import Type.Proxy (Proxy(..))

type Slot = forall q o a. H.Slot q o a

label = Proxy :: Proxy "component"

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
    }
  where

  render :: forall s a. s -> H.ComponentHTML a () m
  render _ =
    HH.div [ className "input-group" ]
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
