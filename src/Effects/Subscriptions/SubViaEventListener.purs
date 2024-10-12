module Effects.Subscriptions.SubViaEventListener where

import Prelude

import Effect.Aff.Class (class MonadAff)

import Data.Maybe (Maybe(..))
import Data.String as String

import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.Event as HQE

import Web.Event.Event as Event
import Web.HTML.HTMLDocument as HTMLDoc
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

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
                         documentObject <- H.liftEffect $ document =<< window
                         H.subscribe' \sid ->
                           HQE.eventListener -- NOTE: uses types from `purescript-web` lib to manually construct event listener on the DOM
                              KET.keyup
                              (HTMLDoc.toEventTarget documentObject)
                              ( ( ( HandleKey sid ) <$> _ ) <<< KE.fromEvent )
                         --pure unit
                         
                      HandleKey _ event -- NOTE: takes _any_ subscription id b/c it's always listening.
                        | KE.shiftKey event -> do
                            H.liftEffect $ Event.preventDefault (KE.toEvent event)
                            let char = KE.key event
                            when (String.length char == 1) do
                              H.modify_ \state -> state { string = state.string <> char }
                            --pure unit
                            
                        | KE.key event == "Enter" -> do
                            H.liftEffect $ Event.preventDefault (KE.toEvent event)
                            H.modify_ _ { string = "" }
                            --pure unit
                            
                        | otherwise ->
                            pure unit
