module Client.Main where

import Prelude

import Client.Data.Music (Note(..), majorScale)

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "🍝"
  log $ show $ majorScale C
