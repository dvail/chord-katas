module Core.Data.Music where

import Prelude
import Data.List (List(..), (:), reverse, drop, dropWhile, fromFoldable)

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

chromaticScale :: Array Note
chromaticScale = [C, Db, D, Eb, E, F, Gb, G, Ab, A, Bb, B]

majorScaleInterval :: Array Interval
majorScaleInterval = [Whole, Whole, Half, Whole, Whole, Whole, Half]

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

