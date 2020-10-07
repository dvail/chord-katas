module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Unit (suite, test, timeout)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  suite "Map and Board Tests" do
    test "todo" do
      Assert.equal 1 1