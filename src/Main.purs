module Main where

import Prelude

import Data.Traversable (for)
import Data.Foldable (for_)
import Effect (Effect)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Console as Console
import Node.Process as Process
import Data.Maybe as Maybe
import Data.Maybe (Maybe(..))
import Data.String.Common as String
import Data.String.Pattern (Pattern(..))
import Effect.Aff (Fiber, Aff)
import Effect.Aff as Aff
import Node.Path as Path
import Data.Array as Array
import Node.Path (FilePath)
import Node.FS.Aff as FS
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Simple.JSON as JSON
import Foreign.Keys (keys) as Foreign
import Foreign (readString, fail) as Foreign
import Foreign (ForeignError(..))
import Foreign.Index (readProp) as Foreign
import Data.Either (Either(..))

type Config =
  { elmHome :: String
  , path :: FilePath
  }

initConfig :: Effect Config
initConfig = { elmHome : _, path : _ }
    <$> getElmHome
    <*> (Maybe.maybe Process.cwd pure =<< flip Array.index 2 <$> Process.argv)
  where
    getHome =
      Maybe.fromMaybe "/root" <$> Process.lookupEnv "HOME"

    getElmHome = do
      home <- getHome
      Maybe.fromMaybe (home <> "/.elm") <$> Process.lookupEnv "ELM_HOME"

type Package =
    -- TODO: split author and name
  { name :: String
  , license :: String
  -- TODO: better version type
  , version :: String
  }

newtype ElmVersion =
  ElmVersion String
derive newtype instance showElmVersion :: Show ElmVersion
derive newtype instance readForeignElmVersion :: JSON.ReadForeign ElmVersion

data Dep = Dep
    { name :: String
    , author :: String
    , version :: String
    }
derive instance genericDep :: Generic Dep _

instance showDep :: Show Dep where
  show = genericShow

newtype Deps =
  Deps (Array Dep)
derive newtype instance showDeps :: Show Deps

unDeps :: Deps -> Array Dep
unDeps (Deps xs) =
  xs

type AppMetaRaw =
  { "elm-version" :: ElmVersion
  , dependencies ::
    { direct :: Deps
    , indirect :: Deps
    }
  }

instance readForeignHasmapStringString :: JSON.ReadForeign Deps where
    readImpl f = do
      keys <- Foreign.keys f
      deps <- for keys $ \key -> do
        ver <- Foreign.readString =<< Foreign.readProp key f
        case Array.uncons $ String.split (Pattern "/") key of
          Nothing ->
            Foreign.fail $ ForeignError $ "Can't parse package name and author from the `" <> key <> "`"

          Just { head:author, tail:tail } ->
            let name = String.joinWith "/" tail
            in pure $ Dep { author: author, name: name, version: ver }

      pure $ Deps deps

getPackagePath :: Config -> ElmVersion -> Dep -> FilePath
getPackagePath config (ElmVersion version) (Dep package) =
  Path.concat
      [ config.elmHome
      , version
      , "packages"
      , package.author
      , package.name
      , package.version
      ]

getAppMeta :: Config -> Aff AppMetaRaw
getAppMeta config = do
    elmJSON <- FS.readTextFile UTF8 $ config.path <> "/elm.json"

    -- TODO: use `either` function
    case JSON.readJSON elmJSON of
      Right (r :: AppMetaRaw) -> do
        pure r

      Left e ->
        Aff.throwError $ Aff.error $ "Can't read elm.json file\n\n" <> show e

getPackageJson :: Config -> ElmVersion -> Dep -> Aff FilePath
getPackageJson config elmVersion dep =
    FS.readTextFile UTF8 $ packagePath <> "/elm.json"
  where
    packagePath =
      getPackagePath config elmVersion dep

getDeps :: Config -> ElmVersion -> Deps -> Aff (Array FilePath)
getDeps config elmVersion deps =
  for (unDeps deps) $
        getPackageJson config elmVersion

main :: Effect (Fiber Unit)
main = do
  config <- initConfig
  Console.log $ show config

  Aff.launchAff do
    elmJSON <- FS.readTextFile UTF8 $ config.path <> "/elm.json"

    app <- getAppMeta config
    let elmVersion = app."elm-version"
    direct <- getDeps config elmVersion app.dependencies.direct
    indirect <- getDeps config elmVersion app.dependencies.indirect

    liftEffect $ Console.log "\nDIRECT:"
    for_ direct $ \file ->
        liftEffect $ case JSON.readJSON file of
          Right (package :: Package) ->
            Console.log $ show package

          Left e ->
            Console.log $ "err reading json: " <> show e

    liftEffect $ Console.log "\nINDIRECT:"
    for_ indirect $ \file ->
        liftEffect $ case JSON.readJSON file of
          Right (package :: Package) ->
            Console.log $ show package

          Left e ->
            Console.log $ "err reading json: " <> show e
