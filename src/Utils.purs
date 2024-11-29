module MyUtils where

import Prelude ((<<<))

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

className :: forall r i. String -> HH.IProp (class :: String | r) i
className = HP.class_ <<< HH.ClassName
