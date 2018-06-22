module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)

import Data.Maybe

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Routing.Hash (matches)

import MammutControl.Components.Router (RouteChange(..), component)
import MammutControl.Routes (routes)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  ui <- runUI component unit body

  _ <- liftEffect $ matches routes \_ new -> do
    launchAff_ $ ui.query $ RouteChange new unit

  pure ui
