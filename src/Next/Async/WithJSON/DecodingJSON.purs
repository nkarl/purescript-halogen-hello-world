module Next.Async.DecodingJSON where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (logShow)

import Halogen as H
import Halogen.HTML as HH

import MyUtils (className)

