module Main where

import Prelude

import App as App
import Control.Monad.Except (runExceptT)
import Data.Array as Array
import Data.Either (either)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Flame (QuerySelector(..))
import Flame.Application.NoEffects as FAN
import GamePass (ProductResponse(..))
import GamePass as GamePass

main :: Effect Unit
main = runAff_ (Console.log <<< either Exception.message show) $
  runExceptT do
    games <- GamePass.queryGameList
    let games' = Array.catMaybes (map (_.id <<< unwrap) games)
    ProductResponse { "Products": products } <- GamePass.queryProductDetails games'
    liftEffect
      $ FAN.mount_ (QuerySelector "body")
      $ App.app
      $ Array.fromFoldable products
