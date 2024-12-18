module Core.Pattern.Input.Child.Node01 where

import Prelude
import Data.Maybe (Maybe(..))

import CSS (backgroundColor, border, borderRadius, padding, solid)
import CSS.Color                                                    as Color
import CSS.Size (em, px)
import Halogen                                                      as H
import Halogen.HTML                                                 as HH
import Halogen.HTML.CSS                                             as HCSS

type Input  = { label :: Int }

type State  = { label :: Int }

data Action = Receive Input

type Slots  = (component :: forall q. H.Slot q Void Unit)

component :: forall q o m. H.Component q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
  where

  initialState :: Input -> State
  initialState { label } = { label }

  render :: forall a. State -> H.ComponentHTML a () m
  render { label } =
    HH.button
      [ HCSS.style do border solid    (px 1.0) (Color.black)
                      borderRadius    (em 0.5) (em 0.5) (em 0.5) (em 0.5)
                      padding         (em 2.0) (em 2.0) (em 2.0) (em 2.0)
                      backgroundColor (Color.hsl 26.0 0.93 0.78)
      ]
      [ HH.text $ show label ]

  handleAction :: Action -> H.HalogenM State Action () o m Unit
  handleAction = case _ of

                     Receive input -> H.modify_ _ { label = input.label }

{--
  Upon receiving input, the child simply updates the state and renders it.
--}
