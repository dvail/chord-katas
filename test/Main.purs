module Test.Main where

import Prelude

import Effect (Effect)
import Test.Core.Data.Music (testScales)

main :: Effect Unit
main = do
  testScales