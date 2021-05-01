module Exceptions where

import Prelude
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Data.Either (either)
import Data.List.Types (NonEmptyList)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Argonaut (JsonDecodeError)
import Foreign (ForeignError)
import Effect.Class.Console (log)
import Effect.Class (class MonadEffect)

_parseDeviceError = SProxy :: SProxy "parseDeviceError"

_parseProjectError = SProxy :: SProxy "parseProjectError"

_parseURLError = SProxy :: SProxy "parseURLError"

_getViewportError = SProxy :: SProxy "getViewportError"

_parseYAMLError = SProxy :: SProxy "parseYAMLError"

_parseJSONError = SProxy :: SProxy "parseJSONError"

type AnyException
  = Variant
      ( parseDeviceError :: String 
      , parseProjectError :: String 
      , parseURLError :: String
      , getViewportError :: String 
      , parseYAMLError :: NonEmptyList ForeignError 
      , parseJSONError :: JsonDecodeError
      , readConfigError :: String
      )

handleAny :: forall m. MonadEffect m => ExceptT AnyException m Unit -> m Unit
handleAny x = do
  action <- runExceptT x
  either (log <<< show) (\_ -> log "success") action
