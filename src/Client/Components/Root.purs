module Client.Components.Root where

import Prelude

import Client.Components.ChromaticScale (mkChromaticScale)
import Client.Components.Progression (mkProgression)
import Client.Components.ScaleDegrees (mkScaleDegrees)
import Core.Data.Music (Note, ScaleDegree)
import Data.List (List(..))
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React

type Props =
  { rootNote :: Note
  , scaleDegrees :: List (ScaleDegree Int)
  }

root :: Component Props
root = do
  chromaticScale <- mkChromaticScale
  scaleDegreesComp <- mkScaleDegrees
  progression <- mkProgression
  component "Root" \initialValue -> React.do
    rootNote /\ setRootNote <- useState initialValue.rootNote
    scaleDegrees /\ setScaleDegrees <- useState initialValue.scaleDegrees
    pure
      $ R.div
        { children:
            [ chromaticScale { currentNote: rootNote, onCurrentNoteChange: setRootNote }
            , scaleDegreesComp { selectedScaleDegrees: scaleDegrees, onSelectedChange: setScaleDegrees }
            , progression { rootNote: rootNote, scaleDegrees: scaleDegrees }
            , R.button
                { children:
                    [ R.text "Clear All"
                    ]
                , onClick: handler_ $ setScaleDegrees \_ -> Nil
                }
            ]
        , className: "flex flex-col items-center text-center"
        }
