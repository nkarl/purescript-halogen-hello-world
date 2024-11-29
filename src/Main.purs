module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

--import Core.Effect.LifeCycles.RandomNumbers as RandomNumbers
--import Core.Effect.Async.HTTPRequests as HTTPRequests
--import Core.Effect.Subscriptinos.Timer as Timer
--import Core.Effect.Subscriptions.SubViaEventListener as SubViaEventListener
--import Core.Pattern.Input.Parent00 as Input.Parent00
--import Core.Pattern.Input.Parent01 as Input.Parent01
--import Core.Pattern.Notify.Parent as Notify.Parent
--import Core.Pattern.Notify.Example as Notify.Example
import Core.Pattern.Query.Parent as Query.Parent
--import Next.Routing.SetHash as SetHash

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
      --SubViaEventListener.component unit body
      --HTTPRequests.component unit body
      --Input.Parent00.component unit body
      --Input.Parent01.component unit body
      --Notify.Parent.component unit body
      --Notify.Example.component unit body
      Query.Parent.component unit body
      --SetHash.component unit body
