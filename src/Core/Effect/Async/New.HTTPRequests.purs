module Core.Effect.Async.HTTPRequestsNew where

import Prelude

import Affjax.RequestHeader              as  AXRH
import Affjax.ResponseFormat             as  AXRF
import Affjax.Web                        as  AXWeb
import Data.HTTP.Method                  as  HTTP.Method

import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))

import Effect.Aff.Class (class MonadAff)

import Halogen                           as  H
import Halogen.HTML                      as  HH
import Halogen.HTML.Events               as  HE
import Halogen.HTML.Properties           as  HP

import Web.Event.Event (Event)
import Web.Event.Event as Event

import MyUtils (className)

type UserName = String

type State =
  { loading  :: Boolean
  , username :: UserName
  , content  :: Maybe String
  }

data Action
  = Capture UserName
  | MakeRequest Event

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
  initialState _ = { loading: false, username: "", content: Nothing }

  handleAction = case _ of
    Capture username -> do
      H.modify_ _ { username = username }

    MakeRequest e -> do
      let
        baseGitHubURL = "https://api.github.com/users/"
      H.liftEffect $ Event.preventDefault e
      username <- H.gets _.username
      let
        userData =
          AXWeb.defaultRequest
            { url            = baseGitHubURL <> username
            , method         = Left HTTP.Method.GET
            , responseFormat = AXRF.string
            , headers        = [ AXRH.ContentType (MediaType "Access-Control-Allow-Origin") ]
            }
      H.modify_ _ { loading = true }
      response <-
        H.liftAff $ AXWeb.request userData
      H.modify_ _ { loading = false, content = map _.body (hush response) }

  render state =
    -- heading title
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
      -- search box
      , HH.div
          [ className "row m-5" ]
          [ HH.div
              [ className "col" ]
              [ HH.form
                  [ className "input-group w-50 container-fluid"
                  , HE.onSubmit \e -> MakeRequest e
                  ]
                  [ HH.span
                      [ className "input-group-text" ]
                      [ HH.text "username" ]
                  , HH.input
                      [ className "form-control"
                      , HP.placeholder "type something to search..."
                      , HE.onValueInput \s -> Capture s
                      ]
                  , HH.button
                      [ className "btn btn-primary" ]
                      [ HH.text "Search" ]
                  ]
              ]
          ]
      -- content panel
      , HH.div
          [ className "row m-5" ]
          [ HH.div
              [ className "col" ]
              [ HH.div
                  [ className "w-50 container-fluid" ]
                  case state.content of
                    Nothing -> []
                    Just responseBody ->
                      [ HH.h2_
                          [ HH.text "Response:" ]
                      , HH.pre
                          []
                          [ HH.code_ [ HH.text responseBody ] ]
                      ]
              ]
          ]
      ]

