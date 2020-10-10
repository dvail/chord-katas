
module Test.Core.Data.Music where

import Prelude

import Core.Data.Music
  (Note(..)
  ,Chord(..)
  ,RomanNumeral(..)
  ,majorScale
  ,majorProgression
  ,minorProgression
  )
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
  suite "Chord progressions" do
    test "Major progressions" do
      Assert.equal ((Major C) : (Major F) : (Major G) : Nil) 
        $ majorProgression C  
        $ I : IV : V : Nil
      Assert.equal ((Major D) : (Major A) : (Major B) : (Minor G) : Nil) 
        $ majorProgression D  
        $ I : V : VI : IV : Nil
      Assert.equal 
        ( (Major G) : (Major G) : (Major G) : (Major G)
        : (Major C) : (Major C) : (Major G) : (Major G)
        : (Major D) : (Major D) : (Major D) : (Major D)
        : Nil
        ) 
        $ majorProgression G  
        $ I : I : I : I : IV : IV : I : I : V : V : I : I : Nil
      Assert.equal ((Major Ab) : (Minor F) : (Major Db) : (Major Eb) : Nil) 
        $ majorProgression Ab 
        $ I : VI : IV : V : Nil
    test "Minor progressions" do
      Assert.equal ((Minor C) : (Minor F) : (Major Ab) : Nil) 
        $ minorProgression C  
        $ I : IV: VI : Nil
      Assert.equal ((Minor D) : (Dim E) : (Minor A) : (Minor D) : Nil) 
        $ minorProgression D  
        $ I : II : V : I : Nil
      Assert.equal ((Minor G) : (Dim A) : (Major Eb) : (Major F) : Nil) 
        $ minorProgression G  
        $ I : II : VI : VII : Nil
      Assert.equal ((Minor Db) : (Minor Gb) : (Major E) : (Major B) : Nil) 
        $ minorProgression Db 
        $ I : IV : III : VII : Nil