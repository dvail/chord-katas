module Client.Components.Progression where

import Prelude

import Client.Style (noteBackgroundClass, noteColorClass)
import Core.Data.Music (Chord, Note, ScaleDegree, chordRoot, displayChord, displayNote, displayScaleDegree, majorProgression)
import Data.Array as Array
import Data.List (List(..), zip)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React

type Props =
  { rootNote :: Note
  , scaleDegrees :: List (ScaleDegree Int)
  }

sdWithChord :: Tuple Chord (ScaleDegree Int) -> React.JSX
sdWithChord (Tuple chord sd) = R.div
  { children:
      [ R.span
          { children: [ R.text $ displayScaleDegree sd ]
          , className: "font-serif text-gray-200 "
          }
      , R.span_ [ R.text $ displayChord chord ]
      ]
  , className: "flex flex-col text-3xl p-4 mr-1 "
      <> "font-bold "
      <> (noteColorClass <<< chordRoot) chord
      <> "bg-white bg-opacity-10"
  }

progression :: Note -> List (ScaleDegree Int) -> React.JSX
progression rootNote scaleDegrees =
  R.div
    { children: Array.fromFoldable $ sdWithChord <$> zipChords chords scaleDegrees
    , className: "flex flex-row justify-center"
    }
  where
  chords = majorProgression rootNote scaleDegrees
  zipChords
    :: Maybe (List Chord)
    -> List (ScaleDegree Int)
    -> List (Tuple Chord (ScaleDegree Int))
  zipChords Nothing _ = Nil
  zipChords (Just cs) sds = zip cs sds

mkProgression :: Component Props
mkProgression = do
  component "Progression" \props -> React.do
    pure
      $ R.div
        { children:
            [ R.h2_ [ R.text "Selected Progression" ]
            , R.h2_
                [ R.text "Key of "
                , R.span
                    { children: [ R.text $ displayNote props.rootNote ]
                    , className: "font-bold" <> noteColorClass props.rootNote
                    }
                ]
            , progression props.rootNote props.scaleDegrees
            ]
        }
