module Core.Data.Music 
  ( Note(..)
  , Chord(..)
  , ScaleDegree
  , majorScale
  , majorProgression
  , minorProgression
  , sd_I
  , sd_II
  , sd_III
  , sd_IV
  , sd_V
  , sd_VI
  , sd_VII
  , chromaticFrom
  , chromaticScale
  ) where

import Prelude

import Data.List (List(..), (:), elem, reverse, drop, dropWhile, fromFoldable, index)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)

data Note = A | Bb | B | C | Db | D | Eb | E | F | Gb | G | Ab

derive instance Eq Note

instance Show Note where
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

instance Show Interval where
  show Whole = "Whole"
  show Half = "Half"

data Chord = Major Note | Minor Note | Dom7 Note | Maj7 Note | Dim Note

derive instance Eq Chord

instance Show Chord where
  show (Major n) = "Major " <> show n
  show (Minor n) = "Minor " <> show n
  show (Dom7 n) = "Dom7 " <> show n
  show (Maj7 n) = "Maj7 " <> show n
  show (Dim n) = "Dim " <> show n

newtype ScaleDegree :: forall k. k -> Type
newtype ScaleDegree i = ScaleDegree Int

-- The 'Int' portion of this type refers to the indexed item of the target scale
sd_I :: ScaleDegree Int
sd_I = ScaleDegree 0
sd_II :: ScaleDegree Int
sd_II = ScaleDegree 1
sd_III :: ScaleDegree Int
sd_III = ScaleDegree 2
sd_IV :: ScaleDegree Int
sd_IV = ScaleDegree 3
sd_V :: ScaleDegree Int
sd_V = ScaleDegree 4
sd_VI :: ScaleDegree Int
sd_VI = ScaleDegree 5
sd_VII :: ScaleDegree Int
sd_VII = ScaleDegree 6

derive instance Eq (ScaleDegree Int)

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
    buildScale' (Whole:ixs) (_:(x:xs)) scale = buildScale' ixs xs $ x:scale
    buildScale' (Half:ixs)  (x:xs)     scale = buildScale' ixs xs $ x:scale
    buildScale' (Half:_)    Nil        scale = scale
    buildScale' (Whole:_)   Nil        scale = scale
    buildScale' (Whole:_)   (_:Nil)    scale = scale
    buildScale' Nil          _         scale = scale

majorScale :: Note -> List Note
majorScale note = buildScale note majorScaleInterval

minorScale :: Note -> List Note
minorScale note = buildScale note minorScaleInterval

type ProgressionChordTypes = 
  { major :: Array (ScaleDegree Int)
  , minor :: Array (ScaleDegree Int)
  , dim   :: Array (ScaleDegree Int)
  }

progression :: 
  (Note -> List Note) ->
  ProgressionChordTypes ->
  Note ->
  List (ScaleDegree Int) ->
  Maybe (List Chord)
progression baseScale chordTypes root numerals = sequence $ chordFromNumeral scale <$> numerals
  where
    chordFromNumeral :: List Note -> ScaleDegree Int -> Maybe Chord
    chordFromNumeral s sd@(ScaleDegree n)
      | elem sd chordTypes.major = Major <$> (index s n)
      | elem sd chordTypes.minor = Minor <$> (index s n)
      | elem sd chordTypes.dim   = Dim   <$> (index s n)
      | otherwise                = Nothing
    scale :: List Note
    scale = baseScale root

majorProgression :: Note -> List (ScaleDegree Int) -> Maybe (List Chord)
majorProgression = progression majorScale 
  { major : [sd_I, sd_IV, sd_V]
  , minor : [sd_II, sd_III, sd_VI]
  , dim   : [sd_VII]
  }

minorProgression :: Note -> List (ScaleDegree Int) -> Maybe (List Chord)
minorProgression = progression minorScale 
  { major : [sd_III, sd_VI, sd_VII]
  , minor : [sd_I, sd_IV, sd_V]
  , dim   : [sd_II]
  }