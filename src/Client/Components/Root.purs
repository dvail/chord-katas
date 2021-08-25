module Client.Components.Root where

import Prelude

import Client.Components.ChromaticScale (mkChromaticScale)
import Core.Data.Music (Note(..), ScaleDegree, majorProgression, sd_I, sd_II, sd_III, sd_IV, sd_V, sd_VI, sd_VII)
import Data.Array as Array
import Data.List (List)
import Data.String (joinWith)
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React


type Props =
  { rootNote :: Note
  , scaleDegrees :: List (ScaleDegree Int)
  }

progression :: Props -> React.JSX
progression { rootNote, scaleDegrees } = 
    R.div
      { children:
        [ R.text $ show $ majorProgression rootNote scaleDegrees]
      }

displayScaleDegree :: ScaleDegree Int -> String
displayScaleDegree x
  | x == sd_I = "I"
  | x == sd_II = "II"
  | x == sd_III = "III"
  | x == sd_IV = "IV"
  | x == sd_V = "V"
  | x == sd_VI = "VI"
  | x == sd_VII = "VII"
  | otherwise = ""

root :: Component Props
root = do
  chromaticScale <- mkChromaticScale
  component "Root" \initialValue -> React.do
    rootNote /\ setRootNote <- useState initialValue.rootNote
    scaleDegrees /\ setScaleDegrees <- useState initialValue.scaleDegrees

    pure
      $ R.div
          { children:
              [ chromaticScale { currentNote: rootNote, onCurrentNoteChange: setRootNote }
              , R.div
                  { children: 
                    [ R.div { children: [ R.text $ "Root: " <> show rootNote ] }
                    , R.div { children: [ R.text $ "Chords: " <> (joinWith " " $ Array.fromFoldable $ displayScaleDegree <$> scaleDegrees) ] }
                    ]
                  }
              , progression initialValue
              ]
          , className: "text-red-500"
          }