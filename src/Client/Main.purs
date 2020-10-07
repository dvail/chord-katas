module Client.Main where

import Data.List
import Prelude

import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Console (log)

data Note = A | Bb | B | C | Db | D | Eb | E | F | Gb | G | Ab

derive instance eqNote :: Eq Note

instance showNotes :: Show Note where
  show A = "A"
  show Bb = "A#/Bb"
  show B = "B"
  show C = "C"
  show Db = "C#/Db"
  show D = "D"
  show Eb = "D#/Eb"
  show E = "E"
  show F = "F"
  show Gb = "F#/Gb"
  show G = "G"
  show Ab = "G#/Ab"

data Interval = Whole | Half
instance showInterval :: Show Interval where
  show Whole = "Whole"
  show Half = "Half"

allNotes :: List Note
allNotes = fromFoldable [C, Db, D, Eb, E, F, Gb, G, Ab, A, Bb, B]

majorScaleInterval :: List Interval
majorScaleInterval = fromFoldable [Whole, Whole, Half, Whole, Whole, Whole, Half]

majorScale :: Note -> List Note
majorScale note = buildScale note majorScaleInterval

allNotesFrom :: Note -> List Note
allNotesFrom rootNote = dropWhile (\n -> n /= rootNote) $ allNotes <> allNotes

buildScale :: Note -> List Interval -> List Note
buildScale rootNote intervals = buildScale' majorScaleInterval (allNotesFrom rootNote) $ Cons rootNote Nil
  where
    buildScale' :: List Interval -> List Note -> List Note -> List Note
    buildScale' (Whole:ixs) (y:(x:xs)) scale = buildScale' ixs xs $ Cons x scale
    buildScale' (Half:ixs)  (x:xs)     scale = buildScale' ixs xs $ Cons x scale
    buildScale' (Half:_)    Nil        scale = scale
    buildScale' (Whole:_)   Nil        scale = scale
    buildScale' (Whole:_)   (x:Nil)    scale = scale
    buildScale' Nil          _         scale = scale

main :: Effect Unit
main = do
  log "🍝"
  log $ show $ majorScale C
