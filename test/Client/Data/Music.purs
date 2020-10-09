
module Test.Core.Data.Music where

import Prelude

import Core.Data.Music (Note(..), majorScale)
import Data.List (List(..), (:), take)
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

testScales :: Effect Unit
testScales = runTest do
  suite "Scale generation" do
    test "Major scales" do
      Assert.equal (C : D : E : F : G : A : B : C : Nil)       $ take 8 (majorScale C)
      Assert.equal (D : E : Gb : G : A : B : Db : D : Nil)     $ take 8 (majorScale D)
      Assert.equal (G : A : B : C : D : E : Gb : G : Nil)      $ take 8 (majorScale G)
      Assert.equal (Ab : Bb : C : Db : Eb : F : G : Ab : Nil)  $ take 8 (majorScale Ab)
      Assert.equal (Db : Eb : F : Gb : Ab : Bb : C : Db : Nil) $ take 8 (majorScale Db)
      Assert.equal (Gb : Ab : Bb : B : Db : Eb : F : Gb : Nil) $ take 8 (majorScale Gb)