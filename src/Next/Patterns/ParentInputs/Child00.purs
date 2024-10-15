module Next.Patterns.ParentInputs.Child00 where

import Prelude

import Halogen as H
import Halogen.HTML as HH

import Type.Proxy (Proxy(..))

{--
  NOTE: This example shows the correct way to insert a child componnent inside a parent component.
    It doesn't do anything more than that.
--}

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
    }

  where

  --render :: ∀ s a. s -> H.ComponentHTML a ()     m
  render   :: ∀ s a. s -> H.ComponentHTML a Slots  m
  render _ =
    HH.div_
      --[ button { label: "Click Me" }
      --[ HH.slot_ (Proxy :: Proxy "button") unit button { label: "Click Me" }
      [ HH.slot_ _ChildComponent unit button { label: "Click Me" } -- NOTE: there is only one child, we use `unit` for the 2nd param (slot).
      ]

--slot_
  --:: ∀ q a i o slots m label slot _1
   --. Cons label (Slot q o slot) _1 slots
  --=> IsSymbol label => Ord slot
  --=> Proxy label
  ---> slot
  ---> (Component q i o m)
  ---> i
  ---> ComponentHTML a slots m

type Input = { label :: String }

type State = { label :: String }

type Slots = ( button :: ∀ q. H.Slot q Void Unit )

-- NOTE: the Proxy produces a "shadow" type parameter without having to specify at least an empty value of that type.
-- https://pursuit.purescript.org/packages/purescript-prelude/6.0.1/docs/Type.Proxy#v:Proxy
-- here, the "shadow" type parameter is a type-leveled string. This parameter technically can be some polymorphic `a` as needed.
_ChildComponent :: Proxy "button" -- a proxy to define the label for the child component and how to identify it.
_ChildComponent =  Proxy 

button :: ∀ q o m. H.Component q Input o m
button =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
  }

  where
    initialState :: Input -> State
    initialState i = i

    render :: forall a. State -> H.ComponentHTML a () m
    render { label } =
      HH.button_
        [ HH.text label
        ]

-- simple HTML renderer; not a component
--button :: ∀ w i. { label :: String } -> HH.HTML w i
--button { label } =
  --HH.button_
    --[ HH.text label
    --]
