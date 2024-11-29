module Core.Pattern.Input.Parent00 where

import Prelude

import Halogen as H
import Halogen.HTML as HH

import Type.Proxy (Proxy(..))
import Core.Pattern.Input.Child.Node00 as Child

-- | the parent component, which is simply a wrapper/container div.
component :: forall q i o m. H.Component q i o m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
  }
  where
  -- the generic types are not used.
  render :: ∀ s a. s -> H.ComponentHTML a Child.Slots m
  render _ =
    HH.div_
      [ HH.slot_ (Proxy :: Proxy "component") unit Child.component { label: "Not Clicked. Click Me!" }
      --[ HH.slot_ _button unit childComponent { label: "Click Me" } -- NOTE: there is only one child, we use `unit` for the 2nd param slot `s`.
      ]

{--
  A simple example to show how to insert a component (child) inside another component (parent).

  In this example, Parent sends some input to Child. Child decides whether to do some work with given input,
  or not. In this case, the Child tracks a stateful integer `count`, which is used to update its button label.
  
  The implementation use 2 structures:
      1. `HH.slot_`
      2. `Proxy :: Proxy "<exact string of the child's symbol>"`

  In this simple example, the parent sends a static label to the child every click. However, we can imagine
  more complex situations where the parent need to process some data and then send the data as input to
  the child. The child then can decide on what action to perform with that given input.
--}

