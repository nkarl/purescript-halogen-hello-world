module Core.Pattern.Query.Parent where

import Prelude

import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import Halogen as H
import Halogen.HTML as HH

import Core.Pattern.Query.Child.Button as Button

type State = { clickCount :: Int }

type WhichButton = Int

data Action = Handle WhichButton Button.Output

type Slots = (button :: Button.Slot)

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }
  where

  initialState _ = { clickCount: 0 }

  render { clickCount } =
    let
      clicks = show clickCount
    in
      HH.div_
        [ HH.slot Button._label 0 Button.component (clicks <> " Enabled") $ Handle 0
        , HH.slot Button._label 1 Button.component (clicks <> " Power"  ) $ Handle 1
        , HH.slot Button._label 2 Button.component (clicks <> " Switch" ) $ Handle 2
        ]

  handleAction = case _ of
    Handle _ output -> case output of
      Button.Clicked -> do
        H.modify_ \s -> s { clickCount = s.clickCount + 1 }
        H.requestAll  Button._label   (Button.GetSwitch) >>= logShow
        H.request     Button._label 1 (Button.GetSwitch) >>= logShow
        H.tell        Button._label 0 (Button.SetSwitch true)

{--
  NOTE: Reasoning about this Example.


    Parent: div                                     Div
    - tracks a state, containing                    /|\    
      - click count                                / | \         is morphism: \state -> (show state) <> label
                                                  /  |  \
                                                 v   v   v       each Bi receives the same equal INITIAL input
    Child : button                              B1  B2  B3       each Bi occupies a slot in the Parent, tracked by an index
    - tracks a state, containing
      - label
      - on/off


  On Child clicked,
                                                    Div <-------------------|
                                                    /|\                     |
                                                   / | \                    |
                                                  /  |  \                   | event sent as output to Parent
                                                 v   v   v                  |
                                                B1  B2  B3 <-----|          |
                                               ------------      | updates its on/off switch
                                                   Click   ------|          |
                                                                            |
                                                   Clicked -----------------|


  On Parent rerendered,
                                        |--------<< Div <-------------------|
                                        |           /|\                     |
                                        |          / | \                    |
              Parent updates its state  |         /  |  \                   |
              and sends that to Child   |        v   v   v                  |
                                        |       B1  B2  B3 <-----|          |
                                        |      ------------      |          |
                                        |          Click   ------|          |
                                        |                                   |
                                        |          Clicked -----------------|
                                        |
                                        |----->>   receive                    always listens for new input


  Adding queries for other purposes
                                                              requestAll -> `Map (slot a)` ----
                                                               /                                \
                                                              /                                  \
                                |---------------> Halogen interface                   unifies to polymorphic `Maybe a` in Child handleQuery
                                |                    :        \                                  /
                                |                    :         \                                /
                                |                    :        tell       -> `Unit` -------------
                                |                    :
                                |                    :
                                |       |--------<< Div <-------------------|
                                |       |           /|\                     |
                                |       |          / | \                    |
                                |       |         /  |  \                   |
            handled in the same logic   |        v   v   v                  |
            as that of Clicked output   |       B1  B2  B3 <-----|          |
                                |       |      ------------      |          |
                                |       |          Click   ------|          |
                                |       |                                   |
                                |       |          Clicked -----------------|
                                |       |
                                |       |----->>   receive
                                |
                                |---------------  Queries
                                                  /     \
                                          SetSwitch     GetSwitch


  NOTE: query questions.

  The output of queries are entirely dependent on the Halogen default interface. There are a certain
  number of actions in the Halogen library, and each defines its own output.

  Therefore, the kind of information requested from the Child must also be restricted by this rule.

  In general there are 2 style: Tell (effectful) and Request (effectful and fetching information from component).
  These combinators operates on predefined Query variants.

  From this example, we see that there are 2 kinds of requests: effectful/forgetful and tracing/faithful.
    - Tell style
      - `tell       -> SetSwitch` is effectful. It just needs done without output.
      - `tellAll` should tell all child to exec some effect.
    - Request style
      - `request` should tell a specific child to perform effect and return its info.
      - `requestAll -> GetSwitch` is tracing. It needs to return the map of all Button slots. 

  NOTE: In this example, both query variants involves the Boolean field state `isEnabled` in the Child. It is
  possible to define other variants to get other field states if needed.

  There are 2 other Halogen interface actions that has similar signatures (dealing with component slots) in
  the Pursuit documentation index:
    - query
    - QueryAll
--}
