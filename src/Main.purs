module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Next.Async.GithubSearch as GithubSearch

{-
  TODO: Make an AppM monad component to wrap all components
-}

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI
      GithubSearch.component unit body
