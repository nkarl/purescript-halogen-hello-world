module Next.Async.GithubSearch.Component.ContentPanel where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import MyUtils (className)
import Type.Proxy (Proxy(..))

type Content = String

type Input = Content

type State =
  { loading :: Boolean
  , content :: Content
  , result :: Maybe String
  }

data Action = Receive Content

type Slot = forall q o. H.Slot q o Unit

label = Proxy :: Proxy "contentPanel"

contentPanel :: forall q o m. H.Component q Input o m
contentPanel = component

component :: forall q o m. H.Component q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
  where
  initialState _ =
    { loading: false
    , content: ""
    , result: Nothing
    }

  handleAction :: Action -> H.HalogenM State Action () o m Unit
  handleAction = case _ of
    Receive content -> do
      H.modify_ \s -> s { content = content }

  render { content } =
    HH.pre
      [ className "container-fluid border border-dark-subtle p-3 rounded" ]
      [ HH.code_ [ HH.text content ] ]
