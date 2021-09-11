module Client.Main where

import Prelude

import Effect (Effect)

import Elmish.Boot (defaultMain)
import Client.ProgressionGenerator (def)

main :: Effect Unit
main = defaultMain { elementId: "app", def }
