module Data.Route where

import Prelude hiding ((/))

import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.UUID as UUID
import Data.UserId (UserId(..))
import Routing.Duplex (RouteDuplex', root, path, as, optional, segment)
import Routing.Duplex.Generic (sum, noArgs)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = LogOn
  | LogOff
  -- | Users (Maybe UserId)
  | Users (Maybe String)

derive instance genericRoute :: Generic Route _

userId :: RouteDuplex' String -> RouteDuplex' UserId
userId = as printer parser
  where
  printer :: UserId -> String
  printer (UserId uuid) = UUID.toString uuid

  parser :: String -> Either String UserId
  parser = UUID.parseUUID >>> map UserId >>> note "Invalid UserId"

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "LogOn": path "logon" noArgs
  , "LogOff": path "logoff" noArgs
  , "Users": "users" / optional segment
  --, "Users": "users" / (optional $ userId segment)
  }
