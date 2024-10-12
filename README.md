This repo collects and documents my notes as I learn Halogen.

The source code is in `/src/` and is organized into 3 steps:

- Basics
    - simple intro to how to make a component in Halogen
- Effects: some use cases such as
    - simple async effect via a REST cycle
    - lifecycle events
    - subscriptions
        - time-based actions (such as in a timer)
            - subscribes to an Emitter
        - reaction to events in the DOM
            - subscribes/listens by an event listener
- Next: patterns for communication between parent-child nodes in the DOM.
    - parent *gives input* to a child.
    - parent queries a child (to do something or to request information).
    - child notifies the parent.


## Basics

### `HalogenHTML`

is the very basic mapping. It writes directly to HTML DOM.

```haskell
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

html =
    HH.div
        [ HP.id "root" ]
        [ HH.input
            [ HP.placeholder "Name" ]
        , HH.button
            [ HP.classes [ HH.ClassName = "btn-primary" ]
            , HP.type_ HP.ButtonSubmit
            ]
            [ HH.text "Submit" ]
        ]
```

There are 2 ways to write it, using `HH.div` or `HH.div_`. The latter is for when we don't need to write any properties.

```haskell
element = HH.div [ ] [ ... ]

element = HH.div_ [ ... ]
```


[This type](https://purescript-halogen.github.io/purescript-halogen/guide/01-Rendering-Halogen-HTML.html#html-types) can take two type parameters `w` for '**widget**' and `i` for '**input**'.

- **widget** describes <u>which components</u> can be used in this HTML node.
- **input** describes the type used to handle DOM events.


### Additionally

`HalogenHTML` has 2 other cousin types.

- `ComponentHTML` when we need to work with *components*.
- `PlainHTML` when we need HTML that doesn't contain components (writes pure HTML).
    - it lets us hide the `w` and `i` type parameters.
    - it needs to be converted with `fromPlainHTML` if we need to combine this type with another that is *event-driven* or *component-driven*.


### `HH.IProp`


## Effects



## Next

### parent gives a child an input


### parent inquires a child (gives a query)


### child notifies the parent
