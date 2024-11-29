module Core.Effect.Subscriptions.SubViaEventListener where

import Prelude

import Effect.Aff.Class (class MonadAff)

import Data.String                          as String
import Data.Maybe (Maybe(..))

import Halogen                              as H
import Halogen.HTML                         as HH
import Halogen.Query.Event                  as HQE

import Web.Event.Event                      as E
import Web.HTML.HTMLDocument                as HTMLDocument
import Web.UIEvent.KeyboardEvent            as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

import Web.HTML (window)
import Web.HTML.Window (document)

type State = { string :: String }

data Action
  = Initialize
  | HandleKey (H.SubscriptionId) (KE.KeyboardEvent) -- NOTE: takes a `subId` and a `kbEvent` to produce an action.

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    }
  }

  where

    initialState :: i -> State
    initialState _ = { string: "" }
  
    render :: State -> H.ComponentHTML Action () m
    render state =
      HH.div_
        [ HH.p_ [ HH.text "Type some characters while holding down the SHIFT key!" ]
        , HH.p_ [ HH.text "Press ENTER or RETURN to clear and remove the event listener" ]
        , HH.p_ [ HH.text state.string ]
        ]
  
    handleAction :: Action -> H.HalogenM State Action () o m Unit
    handleAction = case _ of
                       
                      Initialize -> do
                         doc <- H.liftEffect $ document =<< window
                         H.subscribe'
                            \sid -> HQE.eventListener -- :: ∀ a. EventType -> EventTarget -> (Event -> Maybe a) -> Emitter a
                              (KET.keyup)
                              (HTMLDocument.toEventTarget doc)
                              ( ( ( HandleKey sid ) <$> _ ) <<< KE.fromEvent )
                         --pure unit
                         
                      HandleKey _ e -- NOTE: takes _any_ subscription id b/c it's always listening (already subscribed on init).
                        | KE.shiftKey e -> do
                            H.liftEffect $ E.preventDefault (KE.toEvent e)
                            let char = KE.key e
                            when (String.length char == 1) do
                              H.modify_ \state -> state { string = state.string <> char }
                            --pure unit
                            
                        | KE.key e == "Enter" -> do
                            H.liftEffect $ E.preventDefault (KE.toEvent e)
                            H.modify_ _ { string = "" }
                            --pure unit
                            
                        | otherwise ->
                            pure unit


{--
  NOTE: the hardest part about this example is to track the following points:
    1. a continuous path connecting many PS library implementation from a `HTMLDocument` to an `EventTarget`
    2. a Halogen implementation of this use case as an `Emitter a`
    3. joining 1 and 2 together.
--}

