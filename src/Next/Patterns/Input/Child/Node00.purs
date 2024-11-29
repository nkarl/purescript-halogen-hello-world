module Next.Patterns.Input.Child.Node00 where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Input = { label :: String }

type State = { label :: String, count :: Int }

data Action = Clicked

type Slots = (component :: ∀ q. H.Slot q Void Unit)

{--
  A note on the Proxy type usage.

  Proxy lets us create a "shadow" type parameter without having to specify an empty value of that type.

  Here, the "shadow" type param is a type-level string. This param technically can be some polymorphic
  type `a` as needed.

  https://pursuit.purescript.org/packages/purescript-prelude/6.0.1/docs/Type.Proxy#v:Proxy
--}

-- a proxy to define the label for the child component and how to identify it.
-- following is the "proper" way to do it. Typically this caan be inlined at the parent component's `HH.slot`.
--_button :: Proxy "component" 
--_button = Proxy

-- | the child component, which is a button.
component :: ∀ q o m. H.Component q Input o m
component =
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
      let c = state.count + 1
      -- next line doesn't work. It will distort the button's length with every update. This is because
      -- each update appends to the local component's label. The new label is carried over to the next state.
      --H.modify_ \s -> s { label = s.label <> show c, count = c }
      H.modify_ \s -> s { label = "Click Me! " <> show c <> " clicks", count = c }
