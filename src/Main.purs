module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

--import Effects.LifeCycles.RandomNumbers as RandomNumbers
--import Effects.Async.HTTPRequests as HTTPRequests
--import Effects.Subscriptinos.Timer as Timer
--import Effects.Subscriptions.SubViaEventListener as SubViaEventListener
--import Next.Patterns.Input.Parent00 as Input.Parent00
--import Next.Patterns.Input.Parent01 as Input.Parent01
--import Next.Patterns.Notify.Parent as Notify.Parent
--import Next.Patterns.Notify.Example as Notify.Example
--import Next.Patterns.Query.Parent as Query.Parent
import Next.Patterns.Query.Example as Query.Example
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
      --Query.Parent.component unit body
      Query.Example.component unit body
      --SetHash.component unit body
