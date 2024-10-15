module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

--import Effects.LifeCycles.RandomNumbers as RandomNumbers
--import Effects.Async.HTTPRequests as HTTPRequests
--import Effects.Subscriptinos.Timer as Timer
--import Effects.Subscriptions.SubViaEventListener as SubViaEventListener
--import Next.Patterns.ParentInputs.Child00 as ParentInputs.Child00
--import Next.Patterns.ParentInputs.Child01 as ParentInputs.Child01
--import Next.Patterns.ChildNotifies.Parent as ChildNotifies.Parent
import Next.Patterns.ChildNotifies.Example as ChildNotifies.Example
--import Next.Patterns.ParentQueries.Parent as ParentQueries.Parent

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
      --ParentInputs.Child00.component unit body
      --ParentInputs.Child01.component unit body
      --ChildNotifies.Parent.component unit body
      ChildNotifies.Example.component unit body
      --ParentQueries.Parent.component unit body
