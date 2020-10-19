
module Test.Core.Data.Music where

import Prelude

import Core.Data.Music
  ( Note(..)
  , Chord(..)
  , majorScale
  , majorProgression
  , minorProgression
  , rn_I
  , rn_II
  , rn_III
  , rn_IV
  , rn_V
  , rn_VI
  , rn_VII
  )
import Data.List (List(..), (:), take)
import Data.Maybe (Maybe(..))
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
  suite "Chord progressions" do
    test "Major progressions" do
      Assert.equal (Just ((Major C) : (Major F) : (Major G) : Nil))
        $ majorProgression C  
        $ rn_I : rn_IV : rn_V : Nil
      Assert.equal (Just ((Major D) : (Major A) : (Minor B) : (Major G) : Nil))
        $ majorProgression D  
        $ rn_I : rn_V : rn_VI : rn_IV : Nil
      Assert.equal 
        (Just (
          (Major G) : (Major G) : (Major G) : (Major G)
        : (Major C) : (Major C) : (Major G) : (Major G)
        : (Major D) : (Major D) : (Major G) : (Major G)
        : Nil
        ))
        $ majorProgression G  
        $ rn_I : rn_I : rn_I : rn_I : rn_IV : rn_IV : rn_I : rn_I : rn_V : rn_V : rn_I : rn_I : Nil
      Assert.equal (Just ((Major Ab) : (Minor F) : (Major Db) : (Major Eb) : Nil))
        $ majorProgression Ab 
        $ rn_I : rn_VI : rn_IV : rn_V : Nil
    test "Minor progressions" do
      Assert.equal (Just ((Minor C) : (Minor F) : (Major Ab) : Nil))
        $ minorProgression C  
        $ rn_I : rn_IV : rn_VI : Nil
      Assert.equal (Just ((Minor D) : (Dim E) : (Minor A) : (Minor D) : Nil))
        $ minorProgression D  
        $ rn_I : rn_II : rn_V : rn_I : Nil
      Assert.equal (Just ((Minor G) : (Dim A) : (Major Eb) : (Major F) : Nil))
        $ minorProgression G  
        $ rn_I : rn_II : rn_VI : rn_VII : Nil
      Assert.equal (Just ((Minor Db) : (Minor Gb) : (Major E) : (Major B) : Nil))
        $ minorProgression Db 
        $ rn_I : rn_IV : rn_III : rn_VII : Nil