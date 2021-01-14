module Config (
  serverConf
) where

import Happstack.Server.Internal.Types

serverConf :: Conf
serverConf = Conf {
  port = 3000,
  validator  = Nothing,
  logAccess = Just logMAccess,
  timeout = 120,
  threadGroup = Nothing
}