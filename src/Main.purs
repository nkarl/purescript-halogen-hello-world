module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

--import Effects.LifeCycles.RandomNumbers as RandomNumbers
--import Effects.Async.HTTPRequests as HTTPRequests
--import Effects.Subscriptinos.Timer as Timer
import Effects.Subscriptions.SubViaEventListener as SubViaEventListener
--import Next.Patterns.ParentInputsChild as ParentInputsChild

{-
  TODO: Make an AppM monad component to wrap all components
-}

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI
      --Counter.component unit body
      --RandomNumbers.component unit body
      --Timer.component unit body
      SubViaEventListener.component unit body
      --HTTPRequests.component unit body
      --ParentInputsChild.component unit body
