module Screener.Devices where

import Data.Map (Map, lookup, fromFoldable)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe)
import Toppokki as T

getDevice :: String -> Maybe (Record T.DefaultViewPort)
getDevice key = lookup key deviceMap

deviceMap :: Map String (Record T.DefaultViewPort)
deviceMap =
  fromFoldable
    [ Tuple "widescreen" widescreen
    , Tuple "desktop" desktop
    , Tuple "ipad" ipad
    , Tuple "mobile" mobile
    ]

widescreen :: Record T.DefaultViewPort
widescreen =
  { width: 1920.0
  , height: 1080.0
  , deviceScaleFactor: 1.0
  , isMobile: false
  , isLandscape: false
  , hasTouch: false
  }

desktop :: Record T.DefaultViewPort
desktop =
  { width: 1440.0
  , height: 1024.0
  , deviceScaleFactor: 1.0
  , isMobile: false
  , isLandscape: false
  , hasTouch: false
  }

ipad :: Record T.DefaultViewPort
ipad =
  { width: 1671.0
  , height: 768.0
  , deviceScaleFactor: 1.0
  , isMobile: false
  , isLandscape: true
  , hasTouch: true
  }

mobile :: Record T.DefaultViewPort
mobile =
  { width: 360.0
  , height: 1358.0
  , deviceScaleFactor: 1.0
  , isMobile: true
  , isLandscape: true
  , hasTouch: true
  }
