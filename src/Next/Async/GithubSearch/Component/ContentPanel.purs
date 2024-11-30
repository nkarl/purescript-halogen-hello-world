module Next.Async.GithubSearch.Component.ContentPanel where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import MyUtils (className)
import Type.Proxy (Proxy(..))

type Slot = forall q o. H.Slot q o Unit

label = Proxy :: Proxy "contentPanel"

contentPanel :: forall q i o m. H.Component q i o m
contentPanel = component

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
    HH.div
      [ className "border border-dark-subtle p-3 rounded" ]
      [ HH.text "placeholder JSON" ]