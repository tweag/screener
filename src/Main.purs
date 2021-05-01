module Main where

import Prelude
import Data.Argonaut (JsonDecodeError, decodeJson)
import Data.Either (either)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.String (Replacement(..), Pattern(..), replaceAll)
import Data.Symbol (SProxy(..))
import Data.List.Types (NonEmptyList)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (sequence_)
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Data.Variant (Variant, inj)
import Effect (Effect)
import Control.Monad.Except (withExceptT, except)
import Control.Monad.Except.Trans (runExceptT, ExceptT, lift, throwError)
import Control.Monad.Morph (generalize, hoist)
import Effect.Aff (launchAff_, delay, Aff)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.FS.Aff.Mkdirp (mkdirp)
import Options.Applicative (execParser, str, argument, ParserInfo, Parser, (<**>), info, metavar, fullDesc, progDesc, header, helper)
import Pathy (Dir, File, Path, Rel, unsafePrintPath, currentDir, dir, parseRelDir, parseRelFile, posixParser, posixPrinter, sandboxAny, (<.>), (</>))
import Screener.Devices as D
import Toppokki as T
import Exceptions as E
import Foreign (ForeignError)

-- Configurations
type Config
  = { project :: String
    , urls :: Array String
    , devices :: Array String
    }

type CaptureConfig
  = { project :: String
    , device :: String
    , url :: T.URL
    }

-- parsing
getFolder ::
  forall e m.
  Monad m =>
  String ->
  String ->
  ExceptT
    ( Variant
        ( parseDeviceError :: String
        , parseProjectError :: String
        | e
        )
    )
    m
    (Path Rel Dir)
getFolder project device = do
  pDir <- projectDir
  dDir <- deviceDir
  pure $ currentDir </> screenDir </> pDir </> dDir
  where
  deviceDir = case parseDir device of
    Just x -> pure x
    Nothing -> throwError $ inj E._parseDeviceError $ "Can't parse device " <> device <> "\n"

  projectDir = case parseDir project of
    Just x -> pure x
    Nothing -> throwError $ inj E._parseProjectError $ "Can't parse project " <> project <> "\n"

  screenDir = dir (SProxy :: SProxy "screenshots")

  parseDir :: String -> Maybe (Path Rel Dir)
  parseDir dir = parseRelDir posixParser ("./" <> dir <> "/")

getFname :: forall e m. Monad m => T.URL -> ExceptT (Variant ( parseURLError :: String | e )) m (Path Rel File)
getFname url = (\x -> x <.> "png") <$> fname
  where
  fname = case parseRelFile posixParser (url2fname url) of
    Just x -> pure x
    Nothing -> throwError $ inj E._parseURLError $ "Can't parse url " <> showURL url <> "\n"

getViewport :: forall e m. Monad m => String -> ExceptT (Variant ( getViewportError :: String | e )) m (Record T.DefaultViewPort)
getViewport device = case D.getDevice device of
  Just d -> pure d
  Nothing -> throwError $ inj E._getViewportError $ "Can't find device: " <> device

prepareCapture :: Config -> String -> T.URL -> CaptureConfig
prepareCapture config device url =
  { project: config.project
  , device: device
  , url: url
  }

readConfig ::
  forall e.
  String ->
  ExceptT
    ( Variant
        ( parseYAMLError :: NonEmptyList ForeignError
        , parseJSONError :: JsonDecodeError
        | e
        )
    )
    Aff
    Config
readConfig fpath = do
  content <- lift $ readTextFile UTF8 fpath
  json <- hoist generalize $ withExceptT (inj E._parseYAMLError) (parseYAMLToJson content)
  withExceptT (inj E._parseJSONError) $ except $ decodeJson json

showURL :: T.URL -> String
showURL (T.URL x) = show x

url2fname :: T.URL -> String
url2fname (T.URL x) = (noSlash >>> noColon >>> noDot) x
  where
  noSlash = replaceAll (Pattern "/") (Replacement "_")

  noColon = replaceAll (Pattern ":") (Replacement "_")

  noDot = replaceAll (Pattern ".") (Replacement "_")

capture ::
  forall e.
  T.Page ->
  CaptureConfig ->
  ExceptT
    ( Variant
        ( parseDeviceError :: String
        , parseProjectError :: String
        , parseURLError :: String
        , getViewportError :: String
        | e
        )
    )
    Aff
    Unit
capture page config = do
  folder <- getFolder config.project config.device
  viewport <- getViewport config.device
  fname <- getFname config.url
  lift
    $ do
        log $ "capturing " <> showURL config.url <> " and " <> config.device
        mkdirp (toStringD folder) >>= log
        T.goto config.url page
        T.setViewport viewport page
        delay $ Milliseconds 1000.0
        _ <- T.screenshot { path: toStringF (folder </> fname), fullPage: true } page
        pure unit
  where
  toStringF :: Path Rel File -> String
  toStringF = sandboxAny >>> unsafePrintPath posixPrinter

  toStringD :: Path Rel Dir -> String
  toStringD = sandboxAny >>> unsafePrintPath posixPrinter

cli :: ParserInfo String
cli =
  info (options <**> helper)
    $ fold
        [ fullDesc
        , progDesc "Take screenshots of websites for many devices"
        , header "Screener - take screenshots of websites"
        ]
  where
  options :: Parser String
  options = argument str (metavar "PROJECTCONFIG")

main :: Effect Unit
main = do
  projectFile <- execParser cli
  launchAff_
    $ do
        log "🍝"
        browser <- T.launch { executablePath: "chromium" }
        page <- T.newPage browser
        E.handleAny
          $ do
              config <- readConfig projectFile
              let
                cfgs = prepareCapture config <$> config.devices <*> (T.URL <$> config.urls)

                captures = E.handleAny <$> (capture page <$> cfgs)
              lift $ sequence_ captures
        T.close browser
