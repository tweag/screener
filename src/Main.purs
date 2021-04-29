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
import Options.Applicative (execParser, str, argument, ParserInfo, Parser, (<**>), info, metavar, fullDesc, progDesc, header, helper)
import Toppokki as T

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

getFolder :: String -> String -> Either String (Path Rel Dir)
getFolder project device = case projectDir, deviceDir of
  Left err, _ -> Left $ err
  _, Left err -> Left $ err
  Right a, Right b -> Right $ currentDir </> screenDir </> a </> b
  where
  deviceDir = case parseDir device of
    Just x -> Right x
    Nothing -> Left $ "Can't parse device " <> device <> "\n"

  projectDir = case parseDir project of
    Just x -> Right x
    Nothing -> Left $ "Can't parse project " <> project <> "\n"

  screenDir = dir (SProxy :: SProxy "screenshots")

getFname :: T.URL -> Either String (Path Rel File)
getFname url = case fname of
  Left err -> Left $ err
  Right x -> Right $ x <.> "png"
  where
  fname = case parseRelFile posixParser (url2fname url) of
    Just x -> Right x
    Nothing -> Left $ "Can't parse url " <> showURL url <> "\n"

getViewport :: String -> Either String (Record T.DefaultViewPort)
getViewport device = case D.getDevice device of
  Just d -> Right d
  Nothing -> Left $ "Can't find device: " <> device

prepareCapture :: Config -> String -> T.URL -> CaptureConfig
prepareCapture config device url =
  { project: config.project
  , device: device
  , url: url
  }

parseDir :: String -> Maybe (Path Rel Dir)
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
capture page config = case folderE, fnameE, viewportE of
  Right folder, Right fname, Right viewport -> do
    (mkdirp $ getPathD folder) >>= log
    T.goto config.url page
    T.setViewport viewport page
    delay $ Milliseconds 1000.0
    _ <- T.screenshot { path: getPathF (folder </> fname), fullPage: true } page
    log $ "screenshot taken for " <> showURL config.url <> " and " <> config.device
  Left err, _, _ -> log err
  _, Left err, _ -> log err
  _, _, Left err -> log err
  where
  folderE = getFolder config.project config.device

  fnameE = getFname config.url

  viewportE = getViewport config.device

  getPathF :: Path Rel File -> String
  getPathF = sandboxAny >>> unsafePrintPath posixPrinter

  getPathD :: Path Rel Dir -> String
  getPathD = sandboxAny >>> unsafePrintPath posixPrinter

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
