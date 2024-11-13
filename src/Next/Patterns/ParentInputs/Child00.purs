module Next.Patterns.ParentInputs.Child00 where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Type.Proxy (Proxy(..))

{--
  NOTE: An example to show the correct way to insert a child componnent inside a parent.
    - done via `HH.slot_` and `Proxy :: Proxy "<child's symbol string>"`.
    - The parent sends input to the child. The child then decides to do some work with the input.
    - In this case, the child tracks an stateful integer, which it uses to update its button label.
--}

-- | the parent component, which is simply a wrapper/container div.
component :: forall q i o m. H.Component q i o m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
  }

  where

  -- the generic types are not used.
  render :: ∀ s a. s -> H.ComponentHTML a Slots m
  render _ =
    HH.div_
      [ HH.slot_ (Proxy :: Proxy "childComponent") unit childComponent { label: "Click Me" }
      --[ HH.slot_ _button unit childComponent { label: "Click Me" } -- NOTE: there is only one child, we use `unit` for the 2nd param slot `s`.
      ]

type Input = { label :: String }

type State = { label :: String, count :: Int }

data Action = Clicked

type Slots = (childComponent :: ∀ q. H.Slot q Void Unit)

{--
  The Proxy produces a "shadow" type parameter without having to specify at least an empty value of that type.
  Here, the "shadow" type parameter is a type-leveled string.
  This parameter technically can be some polymorphic `a` as needed.
  https://pursuit.purescript.org/packages/purescript-prelude/6.0.1/docs/Type.Proxy#v:Proxy
--}

--_button :: Proxy "childComponent" -- a proxy to define the label for the child component and how to identify it.
--_button = Proxy

-- | the child component, which is a button.
childComponent :: ∀ q o m. H.Component q Input o m
childComponent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        }
    }

  where
  initialState :: Input -> State
  initialState i = { label: i.label, count: 0 }

  render :: State -> H.ComponentHTML Action () m
  render { label } =
    HH.button
      [ HE.onClick \_ -> Clicked ]
      [ HH.text $ label
      ]

  handleAction = case _ of
    Clicked -> do
      state <- H.get
      let
        c = state.count + 1
      --H.modify_ \s -> s { label = s.label <> show c, count = c } -- will append to childComponent label, distorting the size of the childComponent with every click
      H.modify_ \s -> s { label = "Click Me " <> show c, count = c }
