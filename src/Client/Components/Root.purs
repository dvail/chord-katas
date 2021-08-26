module Client.Components.Root where

import Prelude

import Client.Components.ChromaticScale (mkChromaticScale)
import Client.Components.ScaleDegrees (mkScaleDegrees, scaleDegree)
import Core.Data.Music (Note, ScaleDegree, displayScaleDegree, majorProgression)
import Data.Array as Array
import Data.List (List(..))
import Data.String (joinWith)
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React


type Props =
  { rootNote :: Note
  , scaleDegrees :: List (ScaleDegree Int)
  }

progression :: Note -> List (ScaleDegree Int) -> React.JSX
progression rootNote scaleDegrees = 
    R.div
      { children:
        [ R.text $ show $ majorProgression rootNote scaleDegrees]
      }

root :: Component Props
root = do
  chromaticScale <- mkChromaticScale
  scaleDegreesComp <- mkScaleDegrees
  component "Root" \initialValue -> React.do
    rootNote /\ setRootNote <- useState initialValue.rootNote
    scaleDegrees /\ setScaleDegrees <- useState initialValue.scaleDegrees

    pure
      $ R.div
          { children:
              [ chromaticScale { currentNote: rootNote, onCurrentNoteChange: setRootNote }
              , scaleDegreesComp { selectedScaleDegrees: scaleDegrees, onSelectedChange: setScaleDegrees }
              , R.div
                  { children: 
                    [ R.div 
                      { children: 
                        [ R.text $ "Chords: " <> (joinWith " " $ Array.fromFoldable $ displayScaleDegree <$> scaleDegrees) ] }
                    ]
                  }
              , progression rootNote scaleDegrees
              ]
          , className: "text-red-500"
          }