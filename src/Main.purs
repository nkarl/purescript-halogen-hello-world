module Main where

import Prelude

import Component.HTTPRequests as HTTPRequests
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

-- TODO: Make an AppM monad component to wrap all components

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    --runUI Counter.component unit body
    --runUI RandomNumbers.component unit body
    --runUI Timer.component unit body
    runUI HTTPRequests.component unit body
