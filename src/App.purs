module App
  ( Filters
  , Message
  , Model
  , app
  ) where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Foldable (all)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Number.Format as NF
import Flame (Html)
import Flame.Application.NoEffects (Application)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import GamePass (Category, Platform(..), ProductEntry(..), allCategories, allPlatforms, categoryFromString, encodedCategory, encodedPlatform, platformFromString)

type Model =
  { entries :: Array ProductEntry
  , filters :: Filters
  }

type Filters =
  { category :: Maybe Category
  , platform :: Maybe Platform
  , includeXboxGold :: Boolean
  }

data Message
  = UpdateCategoryFilter String
  | UpdatePlatformFilter String
  | UpdateXboxGoldFilter Boolean

app :: Array ProductEntry -> Application Model Message
app entries =
  { init
  , view
  , update: update
  , subscribe: []
  }
  where
  init =
    { entries: Array.reverse $ Array.sortWith aggregateScore entries
    , filters:
        { category: Nothing
        , platform: Nothing
        , includeXboxGold: false
        }
    }

  view model = HE.main "main"
    [ filtersView model.filters
    , gameListView model
    ]

  update model = case _ of
    UpdateCategoryFilter "Any category" ->
      model { filters = model.filters { category = Nothing } }
    UpdateCategoryFilter category ->
      model { filters = model.filters { category = categoryFromString category } }
    UpdatePlatformFilter "Any platform" ->
      model { filters = model.filters { platform = Nothing } }
    UpdatePlatformFilter platform ->
      model { filters = model.filters { platform = platformFromString platform } }
    UpdateXboxGoldFilter includeXboxGold ->
      model { filters = model.filters { includeXboxGold = includeXboxGold } }

filtersView :: Filters -> Html Message
filtersView filters =
  HE.div [ HA.class' "fixed w-full z-10 top-0 bg-zinc-800 p-2 drop-shadow-md" ]
    [ HE.div [ HA.class' "flex flex-wrap gap-2 items-center container mx-auto px-8 text-sm" ]
        [ HE.select
            [ HA.value $ maybe "Any platform" encodedPlatform filters.platform
            , HA.onInput UpdatePlatformFilter
            ]
            $ map optionView ("Any platform" : map encodedPlatform allPlatforms)
        , HE.select
            [ HA.value $ maybe "Any category" encodedCategory filters.category
            , HA.onInput UpdateCategoryFilter
            ]
            $ map optionView ("Any category" : map encodedCategory allCategories)
        , HE.select [ HA.disabled true ]
            [ HE.option_ [ HE.text "Sort by Aggregate score" ] ]
        , HE.input [ HA.type' "checkbox" ]
        , HE.label [ HA.onCheck UpdateXboxGoldFilter ]
            [ HE.text "Include Xbox Gold" ]
        ]
    ]

  where
  optionView e =
    HE.option [ HA.value e ] [ HE.text e ]

gameListView :: forall a. Model -> Html a
gameListView model =
  HE.div [ HA.class' "list-view" ]
    [ HE.div [ HA.class' "flex flex-wrap gap-4" ]
        $ map gameEntryView
        $ Array.filter (createFilter model.filters)
        $ model.entries
    ]
  where
  createFilter :: Filters -> ProductEntry -> Boolean
  createFilter filters =
    all (\platform -> Array.elem (encodedPlatform platform) <<< _."AllowedPlatforms" <<< unwrap) filters.platform
      && all (\cat -> Array.elem (encodedCategory cat) <<< _."Categories" <<< unwrap) filters.category
      && (((||) filters.includeXboxGold) <<< not <<< _."RequiresXboxLiveGold" <<< unwrap)

gameEntryView :: forall a. ProductEntry -> Html a
gameEntryView (ProductEntry entry) =
  HE.div [ HA.class' "flex flex-col relative w-72 h-42 rounded-2xl drop-shadow-md overflow-hidden" ]
    [ HE.img [ HA.src $ entry."ImageTile"."URI", HA.class' "absolute blur-sm" ]
    , HE.div [ HA.class' "p-2 z-10 bg-zinc-900/60" ]
        [ HE.a
            [ HA.href $ "https://www.xbox.com/en-gb/games/store/_/" <> unwrap entry."StoreId"
            , HA.class' "flex items-center gap-2"
            ]
            $ Array.snoc (map platformIcon entry."AllowedPlatforms")
            $ HE.p [ HA.class' "text-lg font-bold clip-text" ] [ HE.text $ entry."ProductTitle" ]
        , HE.p [ HA.class' "text-sm clip-text" ] [ HE.text $ entry."DeveloperName" ]
        , HE.p [ HA.class' "text-sm clip-text" ] [ HE.text $ "Review score: " <> show entry."ReviewScore" ]
        , HE.p [ HA.class' "text-sm clip-text" ] [ HE.text $ "Review count: " <> show (Int.trunc entry."ReviewCount") ]
        , HE.p [ HA.class' "text-sm clip-text" ] [ HE.text $ "Aggregate score: " <> NF.toStringWith (NF.precision 5) (aggregateScore (ProductEntry entry)) ]
        ]
    ]
  where
  platformIcon platform =
    case platformFromString platform of
      Just WindowsDesktop -> HE.div' [ HA.class' "windows icon" ]
      Just WindowsXbox -> HE.div' [ HA.class' "xbox icon" ]
      _ -> HE.createEmptyElement "span"

aggregateScore :: ProductEntry -> Number
aggregateScore (ProductEntry entry) =
  (entry."ReviewCount" / (entry."ReviewCount" + min)) * entry."ReviewScore" + (min / (entry."ReviewCount" + min))
  where
  min = 100.0
