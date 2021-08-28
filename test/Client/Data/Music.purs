
module Test.Core.Data.Music where

import Prelude

import Core.Data.Music
  ( Note(..)
  , Chord(..)
  , majorScale
  , majorProgression
  , minorProgression
  , sdI
  , sdII
  , sdIII
  , sdIV
  , sdV
  , sdVI
  , sdVII
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
        $ sdI : sdIV : sdV : Nil
      Assert.equal (Just ((Major D) : (Major A) : (Minor B) : (Major G) : Nil))
        $ majorProgression D  
        $ sdI : sdV : sdVI : sdIV : Nil
      Assert.equal 
        (Just (
          (Major G) : (Major G) : (Major G) : (Major G)
        : (Major C) : (Major C) : (Major G) : (Major G)
        : (Major D) : (Major D) : (Major G) : (Major G)
        : Nil
        ))
        $ majorProgression G  
        $ sdI : sdI : sdI : sdI : sdIV : sdIV : sdI : sdI : sdV : sdV : sdI : sdI : Nil
      Assert.equal (Just ((Major Ab) : (Minor F) : (Major Db) : (Major Eb) : Nil))
        $ majorProgression Ab 
        $ sdI : sdVI : sdIV : sdV : Nil
    test "Minor progressions" do
      Assert.equal (Just ((Minor C) : (Minor F) : (Major Ab) : Nil))
        $ minorProgression C  
        $ sdI : sdIV : sdVI : Nil
      Assert.equal (Just ((Minor D) : (Dim E) : (Minor A) : (Minor D) : Nil))
        $ minorProgression D  
        $ sdI : sdII : sdV : sdI : Nil
      Assert.equal (Just ((Minor G) : (Dim A) : (Major Eb) : (Major F) : Nil))
        $ minorProgression G  
        $ sdI : sdII : sdVI : sdVII : Nil
      Assert.equal (Just ((Minor Db) : (Minor Gb) : (Major E) : (Major B) : Nil))
        $ minorProgression Db 
        $ sdI : sdIV : sdIII : sdVII : Nil