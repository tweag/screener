module Main where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Argonaut (decodeJson, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Replacement(..), Pattern(..), replaceAll)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (sequence_)
import Data.Foldable (fold)
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Effect (Effect)
import Effect.Aff (launchAff_, delay, Aff)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.FS.Aff.Mkdirp (mkdirp)
import Pathy (Dir, File, Path, Rel, unsafePrintPath, currentDir, dir, parseRelDir, parseRelFile, posixParser, posixPrinter, sandboxAny, (<.>), (</>))
import Screener.Devices as D
import Options.Applicative (execParser, str, argument, ParserInfo, Parser, (<**>), info, strOption, long, metavar, help, fullDesc, progDesc, header, helper)
import Toppokki as T

type Config
  = { project :: String
    , urls :: Array String
    , devices :: Array String
    }

type CaptureConfig
  = { project :: String
    , device :: String
    , viewport :: Either String (Record T.DefaultViewPort)
    , folder :: Either String (Path Rel Dir)
    , fname :: Either String (Path Rel File)
    , url :: T.URL
    }

prepareCapture :: Config -> String -> T.URL -> CaptureConfig
prepareCapture config device url =
  { project: config.project
  , folder:
      case projectDir, deviceDir of
        Left err, _ -> Left $ err
        _, Left err -> Left $ err
        Right a, Right b -> Right $ currentDir </> screenDir </> a </> b
  , fname:
      case fname of
        Left err -> Left $ err
        Right x -> Right $ x <.> "png"
  , viewport:
      case D.getDevice device of
        Just d -> Right d
        Nothing -> Left $ "Can't find device: " <> device
  , device: device
  , url: url
  }
  where
  screenDir = dir (SProxy :: SProxy "screenshots")

  deviceDir = case parseDir device of
    Just x -> Right x
    Nothing -> Left $ "Can't parse device " <> device <> "\n"

  projectDir = case parseDir config.project of
    Just x -> Right x
    Nothing -> Left $ "Can't parse project " <> config.project <> "\n"

  fname = case parseRelFile posixParser (url2fname url) of
    Just x -> Right x
    Nothing -> Left $ "Can't parse url " <> showURL url <> "\n"

  parseDir dir = parseRelDir posixParser ("./" <> dir <> "/")

readConfig :: String -> Aff (Either String Config)
readConfig fpath = do
  content <- readTextFile UTF8 fpath
  case runExcept $ parseYAMLToJson content of
    Left err -> pure $ Left "Could not parse YAML file"
    Right json ->
      pure
        $ case decodeJson json of
            Left err -> Left $ printJsonDecodeError err
            Right config -> Right config

showURL :: T.URL -> String
showURL (T.URL x) = show x

url2fname :: T.URL -> String
url2fname (T.URL x) = (noSlash >>> noColon >>> noDot) x
  where
  noSlash = replaceAll (Pattern "/") (Replacement "_")

  noColon = replaceAll (Pattern ":") (Replacement "_")

  noDot = replaceAll (Pattern ".") (Replacement "_")

capture :: T.Page -> CaptureConfig -> Aff Unit
capture page config = case config of
  { viewport: Left err } -> log err
  { folder: Left err } -> log err
  { fname: Left err } -> log err
  { viewport: Right viewport, folder: Right folder, fname: Right fname } -> do
    (mkdirp $ getPathD folder) >>= log
    T.goto config.url page
    T.setViewport viewport page
    delay $ Milliseconds 1000.0
    _ <- T.screenshot { path: getPathF (folder </> fname), fullPage: true } page
    log $ "screenshots for " <> showURL config.url <> " and " <> config.device <> " taken"
    where
    getPathF :: Path Rel File -> String
    getPathF = sandboxAny >>> unsafePrintPath posixPrinter

    getPathD :: Path Rel Dir -> String
    getPathD = sandboxAny >>> unsafePrintPath posixPrinter


cli :: ParserInfo String
cli = info (options <**> helper) $ fold
         [fullDesc,
           progDesc "Take screenshots of websites for many devices",
           header "Screener - take screenshots of websites"]
    where
    options :: Parser String
    options = argument str (metavar "PROJECTCONFIG")


main :: Effect Unit
main = do
        projectFile <- execParser cli
        launchAff_ do
          log "🍝"
          readConfig projectFile
            >>= case _ of
                Left err -> log err
                Right config -> do
                  browser <- T.launch { executablePath: "chromium" }
                  page <- T.newPage browser
                  let
                    cfgs = prepareCapture config <$> config.devices <*> (T.URL <$> config.urls)
                  sequence_ (capture page <$> cfgs)
                  T.close browser
