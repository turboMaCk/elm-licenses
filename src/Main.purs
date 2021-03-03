module Main where

import Prelude

import Data.Generic.Rep as GR
import Data.Traversable
import Effect (Effect)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
-- import Data.HashMap (HashMap)
-- import Data.HashMap as HashMap
import Effect.Console as Console
import Node.Process as Process
import Data.Maybe as Maybe
import Effect.Aff (Fiber, Aff)
import Effect.Aff as Aff
import Node.Path as Path
import Data.Array as Array
import Node.Path (FilePath)
import Node.FS.Aff as FS
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Simple.JSON as JSON
import Foreign.Keys as Foreign
import Foreign as Foreign
import Foreign.Index as Foreign
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

type PackageMeta =
  { name :: String
  , author :: String
  -- TODO: better version
  , version :: String
  }

newtype ElmVersion =
  ElmVersion String
derive newtype instance showElmVersion :: Show ElmVersion
derive newtype instance readForeignElmVersion :: JSON.ReadForeign ElmVersion

data Dep = Dep
    { name :: String
    , version :: String
    }
derive instance genericDep :: Generic Dep _

instance showDep :: Show Dep where
  show = genericShow

newtype Deps =
  Deps (Array Dep)
derive newtype instance showDeps :: Show Deps

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
        pure $ Dep { name: key, version: ver }

      pure $ Deps deps

getPackagePath :: Config -> ElmVersion -> PackageMeta -> FilePath
getPackagePath config (ElmVersion version) package =
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

    case JSON.readJSON elmJSON of
      Right (r :: AppMetaRaw) -> do
        pure r

      Left e ->
        Aff.throwError $ Aff.error $ "Can't read elm.json file\n\n" <> show e

getPackageJson :: Config -> AppMetaRaw -> Aff FilePath
getPackageJson config appMeta =
    FS.readTextFile UTF8 $
        (getPackagePath config appMeta."elm-version"
                        { author: "elm"
                        , name: "core"
                        , version: "1.0.5"
                        }
        ) <> "/elm.json"

main :: Effect (Fiber Unit)
main = do
  config <- initConfig
  Console.log $ show config

  Aff.launchAff do
    elmJSON <- FS.readTextFile UTF8 $ config.path <> "/elm.json"

    app <- getAppMeta config
    liftEffect $ Console.log $ show app
    file <- getPackageJson config app

    liftEffect $ case JSON.readJSON file of
      Right (package :: Package) ->
        Console.log $ show package

      Left e ->
        Console.log $ "err reading json: " <> show e
