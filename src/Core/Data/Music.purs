module Core.Data.Music 
  (
    Note(..)
  , Chord(..)
  , RomanNumeral
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
  ) where

import Prelude
import Data.List (List(..), (:), elem, reverse, drop, dropWhile, fromFoldable, index)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)

data Note = A | Bb | B | C | Db | D | Eb | E | F | Gb | G | Ab

derive instance eqNote :: Eq Note

instance showNotes :: Show Note where
  show A = "A"
  show Bb = "Bb"
  show B = "B"
  show C = "C"
  show Db = "Db"
  show D = "D"
  show Eb = "Eb"
  show E = "E"
  show F = "F"
  show Gb = "Gb"
  show G = "G"
  show Ab = "Ab"

data Interval = Whole | Half

instance showInterval :: Show Interval where
  show Whole = "Whole"
  show Half = "Half"

data Chord = Major Note | Minor Note | Dom7 Note | Maj7 Note | Dim Note

derive instance eqChord :: Eq Chord

instance showChord :: Show Chord where
  show (Major n) = "Major " <> show n
  show (Minor n) = "Minor " <> show n
  show (Dom7 n) = "Dom7 " <> show n
  show (Maj7 n) = "Maj7 " <> show n
  show (Dim n) = "Dim " <> show n

{- TODO Hide this export to prevent construction -}
newtype RomanNumeral i = RomanNumeral Int

rn_I :: RomanNumeral Int
rn_I = RomanNumeral 1
rn_II :: RomanNumeral Int
rn_II = RomanNumeral 2
rn_III :: RomanNumeral Int
rn_III = RomanNumeral 3
rn_IV :: RomanNumeral Int
rn_IV = RomanNumeral 4
rn_V :: RomanNumeral Int
rn_V = RomanNumeral 5
rn_VI :: RomanNumeral Int
rn_VI = RomanNumeral 6
rn_VII :: RomanNumeral Int
rn_VII = RomanNumeral 7

chromaticScale :: Array Note
chromaticScale = [C, Db, D, Eb, E, F, Gb, G, Ab, A, Bb, B]

majorScaleInterval :: Array Interval
majorScaleInterval = [Whole, Whole, Half, Whole, Whole, Whole, Half]

minorScaleInterval :: Array Interval
minorScaleInterval = [Whole, Half, Whole, Whole, Half, Whole, Whole]

chromaticFrom :: Note -> List Note
chromaticFrom rootNote = dropWhile (\n -> n /= rootNote) $ chromaticList <> chromaticList
  where chromaticList = reverse $ fromFoldable chromaticScale

buildScale :: Note -> Array Interval -> List Note
buildScale rootNote intervals = buildScale' intervalList noteList $ rootNote:Nil
  where
    noteList :: List Note
    -- since we already have the root note added to the accumulator list, drop it from the working note list
    noteList = drop 1 $ chromaticFrom rootNote
    intervalList :: List Interval
    intervalList = reverse $ fromFoldable intervals
    buildScale' :: List Interval -> List Note -> List Note -> List Note
    buildScale' (Whole:ixs) (y:(x:xs)) scale = buildScale' ixs xs $ x:scale
    buildScale' (Half:ixs)  (x:xs)     scale = buildScale' ixs xs $ x:scale
    buildScale' (Half:_)    Nil        scale = scale
    buildScale' (Whole:_)   Nil        scale = scale
    buildScale' (Whole:_)   (x:Nil)    scale = scale
    buildScale' Nil          _         scale = scale

majorScale :: Note -> List Note
majorScale note = buildScale note majorScaleInterval

minorScale :: Note -> List Note
minorScale note = buildScale note minorScaleInterval

majorProgression :: Note -> List (RomanNumeral Int) -> Maybe (List Chord)
majorProgression root numerals = sequence $ chordFromNumeral scale <$> numerals
  where
    chordFromNumeral :: List Note -> RomanNumeral Int -> Maybe Chord
    chordFromNumeral s (RomanNumeral n) 
      | elem n [1 ,4, 5] = Major <$> (index s $ n - 1)
      | elem n [2, 3, 6] = Minor <$> (index s $ n - 1)
      | elem n [7]       = Dim   <$> (index s $ n - 1)
      | otherwise = Nothing
    scale :: List Note
    scale = majorScale root

minorProgression :: Note -> List (RomanNumeral Int) -> Maybe (List Chord)
minorProgression root numerals = sequence $ chordFromNumeral scale <$> numerals
  where
    chordFromNumeral :: List Note -> RomanNumeral Int -> Maybe Chord
    chordFromNumeral s (RomanNumeral n) 
      | elem n [3, 6, 7] = Major <$> (index s $ n - 1)
      | elem n [1, 4, 5] = Minor <$> (index s $ n - 1)
      | elem n [2]       = Dim   <$> (index s $ n - 1)
      | otherwise = Nothing
    scale :: List Note
    scale = minorScale root

