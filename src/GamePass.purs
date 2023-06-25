module GamePass
  ( Category(..)
  , Error(..)
  , Image
  , Item(..)
  , Platform(..)
  , PlatformSelector(..)
  , ProductEntry(..)
  , ProductId(..)
  , ProductResponse(..)
  , allCategories
  , allPlatforms
  , categoryFromString
  , encodedCategory
  , encodedPlatform
  , encodedPlatformSelector
  , platformFromString
  , queryGameList
  , queryProductDetails
  ) where

import Prelude

import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (json)
import Affjax.Web (Request)
import Affjax.Web as Affjax
import Control.Monad.Except (ExceptT(..), except, withExceptT)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError, decodeJson, encodeJson)
import Data.Array as Array
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Foreign.Object (Object)

data PlatformSelector
  = Console
  | PC
  | EaPlay
  | All

encodedPlatformSelector :: PlatformSelector -> String
encodedPlatformSelector = case _ of
  Console -> "f6f1f99f-9b49-4ccd-b3bf-4d9767a77f5e"
  PC -> "fdd9e2a7-0fee-49f6-ad69-4354098401ff"
  EaPlay -> "fdd9e2a7-0fee-49f6-ad69-4354098401ff"
  All -> "29a81209-df6f-41fd-a528-2ae6b91f719c"

newtype Item = Item
  { id :: Maybe ProductId }

derive newtype instance DecodeJson Item
derive instance Newtype Item _

newtype ProductId = ProductId String

derive newtype instance DecodeJson ProductId
derive newtype instance EncodeJson ProductId
derive instance Newtype ProductId _

newtype ProductEntry = ProductEntry
  { "ProductTitle" :: String
  , "ProductType" :: String
  , "DeveloperName" :: String
  , "PublisherName" :: String
  , "ReviewScore" :: Number
  , "ReviewCount" :: Number
  , "Categories" :: Array String
  , "AllowedPlatforms" :: Array String
  , "RequiresXboxLiveGold" :: Boolean
  , "ImageTile" :: Image
  , "ImageHero" :: Maybe Image
  , "ImagePoster" :: Image
  , "StoreId" :: ProductId
  }

derive newtype instance DecodeJson ProductEntry
derive instance Newtype ProductEntry _

type Image =
  { "URI" :: String
  , "Width" :: Number
  , "Height" :: Number
  }

newtype ProductResponse = ProductResponse
  { "Products" :: Object ProductEntry
  }

derive newtype instance DecodeJson ProductResponse

data Category
  = ActionAdventure
  | RolePlay
  | Strategy
  | Shooter
  | FamilyAndKids
  | Classics
  | Platformer
  | RacingAndFlying
  | Other

allCategories :: Array Category
allCategories =
  [ ActionAdventure, RolePlay, Strategy, Shooter, FamilyAndKids, Classics, Platformer, RacingAndFlying, Other ]

encodedCategory :: Category -> String
encodedCategory = case _ of
  ActionAdventure -> "Action & adventure"
  RolePlay -> "Role playing"
  Strategy -> "Strategy"
  Shooter -> "Shooter"
  FamilyAndKids -> "Family & kids"
  Classics -> "Classics"
  Platformer -> "Platformer"
  RacingAndFlying -> "Racing & flying"
  Other -> "Other"

categoryFromString :: String -> Maybe Category
categoryFromString str = Array.find (((==) str) <<< encodedCategory) allCategories

data Platform
  = WindowsXbox
  | WindowsDesktop

allPlatforms :: Array Platform
allPlatforms =
  [ WindowsXbox, WindowsDesktop ]

encodedPlatform :: Platform -> String
encodedPlatform = case _ of
  WindowsXbox -> "Windows.Xbox"
  WindowsDesktop -> "Windows.Desktop"

platformFromString :: String -> Maybe Platform
platformFromString str = Array.find (((==) str) <<< encodedPlatform) allPlatforms

data Error
  = AffjaxError Affjax.Error
  | JsonError JsonDecodeError

instance Show Error where
  show = case _ of
    AffjaxError err -> Affjax.printError err
    JsonError err -> show err

queryGameList :: ExceptT Error Aff (Array Item)
queryGameList =
  runRequest $ Affjax.defaultRequest
    { url = "https://corsproxy.io?https://catalog.gamepass.com/sigls/v2?id=29a81209-df6f-41fd-a528-2ae6b91f719c&language=en-us&market=US"
    , responseFormat = json
    }

queryProductDetails :: Array ProductId -> ExceptT Error Aff ProductResponse
queryProductDetails ids = do
  runRequest $ Affjax.defaultRequest
    { method = Left POST
    , url = "https://corsproxy.io?https://catalog.gamepass.com/products?market=US&language=en-US&hydration=MobileDetailsForConsole"
    , responseFormat = json
    , content = Just $ Json $ encodeJson { "Products": ids }
    }

runRequest :: forall a. DecodeJson a => Request Json -> ExceptT Error Aff a
runRequest req = do
  res <- withExceptT AffjaxError $ ExceptT $ Affjax.request req
  withExceptT JsonError $ except $ decodeJson res.body
