module Core.Data.Music
  ( Note(..)
  , Chord(..)
  , ScaleDegree(..)
  , majorScale
  , majorProgression
  , minorProgression
  , chordRoot
  , chromaticFrom
  , chromaticScale
  , displayChord
  , displayNote
  , displayScaleDegree
  ) where

import Prelude

import Data.List (List(..), (:), elem, reverse, drop, dropWhile, fromFoldable, index)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replace)
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

chordRoot :: Chord -> Note
chordRoot (Major n) = n
chordRoot (Minor n) = n
chordRoot (Dom7 n) = n
chordRoot (Maj7 n) = n
chordRoot (Dim n) = n

data ScaleDegree = I | II | III | IV | V | VI | VII

derive instance Eq ScaleDegree

sdToIndex :: ScaleDegree -> Int
sdToIndex I = 0
sdToIndex II = 1
sdToIndex III= 2
sdToIndex IV = 3
sdToIndex V = 4
sdToIndex VI = 5
sdToIndex VII = 6

displayScaleDegree :: ScaleDegree -> String
displayScaleDegree I = "I"
displayScaleDegree II = "II"
displayScaleDegree III = "III"
displayScaleDegree IV = "IV"
displayScaleDegree V = "V"
displayScaleDegree VI = "VI"
displayScaleDegree VII = "VII"

displayNote :: Note -> String
displayNote = show >>> replace (Pattern "b") (Replacement "♭")

displayChord :: Chord -> String
displayChord (Major n) = displayNote n
displayChord (Minor n) = displayNote n <> "m"
displayChord (Dom7 n) = displayNote n <> "dom⁷"
displayChord (Maj7 n) = displayNote n <> "maj⁷"
displayChord (Dim n) = displayNote n <> "°"

chromaticScale :: Array Note
chromaticScale = [ C, Db, D, Eb, E, F, Gb, G, Ab, A, Bb, B ]

majorScaleInterval :: Array Interval
majorScaleInterval = [ Whole, Whole, Half, Whole, Whole, Whole, Half ]

minorScaleInterval :: Array Interval
minorScaleInterval = [ Whole, Half, Whole, Whole, Half, Whole, Whole ]

chromaticFrom :: Note -> List Note
chromaticFrom rootNote = dropWhile (\n -> n /= rootNote) $ chromaticList <> chromaticList
  where
  chromaticList = reverse $ fromFoldable chromaticScale

buildScale :: Note -> Array Interval -> List Note
buildScale rootNote intervals = buildScale' intervalList noteList $ rootNote : Nil
  where
  noteList :: List Note
  -- since we already have the root note added to the accumulator list, drop it from the working note list
  noteList = drop 1 $ chromaticFrom rootNote
  intervalList :: List Interval
  intervalList = reverse $ fromFoldable intervals
  buildScale' :: List Interval -> List Note -> List Note -> List Note
  buildScale' (Whole : ixs) (_ : (x : xs)) scale = buildScale' ixs xs $ x : scale
  buildScale' (Half : ixs) (x : xs) scale = buildScale' ixs xs $ x : scale
  buildScale' (Half : _) Nil scale = scale
  buildScale' (Whole : _) Nil scale = scale
  buildScale' (Whole : _) (_ : Nil) scale = scale
  buildScale' Nil _ scale = scale

majorScale :: Note -> List Note
majorScale note = buildScale note majorScaleInterval

minorScale :: Note -> List Note
minorScale note = buildScale note minorScaleInterval

-- TODO There should be a way to remove all instances of `Maybe` in the below code
-- Might require using something other than a `List` for the incoming scale
progression
  :: (Note -> List Note)
  -> (ScaleDegree -> Note -> Chord)
  -> Note
  -> List ScaleDegree
  -> Maybe (List Chord)
progression baseScale chordFor root numerals = sequence $ chordFromNumeral scale <$> numerals
  where
  chordFromNumeral :: List Note -> ScaleDegree -> Maybe Chord
  chordFromNumeral s sd 
    | Just note <- (index s $ sdToIndex sd) = pure $ chordFor sd note
    | otherwise = Nothing
  scale :: List Note
  scale = baseScale root

majorProgressionChord :: ScaleDegree -> Note -> Chord
majorProgressionChord I n = Major n
majorProgressionChord II n = Minor n
majorProgressionChord III n = Minor n
majorProgressionChord IV n = Major n
majorProgressionChord V n = Major n
majorProgressionChord VI n = Minor n
majorProgressionChord VII n = Dim n

minorProgressionChord :: ScaleDegree -> Note -> Chord
minorProgressionChord I n = Minor n
minorProgressionChord II n = Dim n
minorProgressionChord III n = Major n
minorProgressionChord IV n = Minor n
minorProgressionChord V n = Minor n
minorProgressionChord VI n = Major n
minorProgressionChord VII n = Major n

majorProgression :: Note -> List ScaleDegree -> Maybe (List Chord)
majorProgression = progression majorScale majorProgressionChord

minorProgression :: Note -> List ScaleDegree -> Maybe (List Chord)
minorProgression = progression minorScale minorProgressionChord
