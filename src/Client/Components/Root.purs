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
            [ R.h1
                { children: [ R.text "Chord Katas" ]
                , className: "text-7xl font-display mt-6 text-gray-300"
                }
            , R.h2
                { children: [ R.text "Chord Progression Generator" ]
                , className: "text-lg mb-8 text-gray-200"
                }
            , chromaticScale
                { currentNote: rootNote, onCurrentNoteChange: setRootNote }
            , scaleDegreesComp
                { selectedScaleDegrees: scaleDegrees, onSelectedChange: setScaleDegrees }
            , R.div
                { children:
                    [ progression { rootNote: rootNote, scaleDegrees: scaleDegrees } ]
                , className: "flex flex-col flex-1 justify-center "
                }
            , R.button
                { children: [ R.text "Clear All" ]
                , onClick: handler_ $ setScaleDegrees \_ -> Nil
                , className: "px-4 py-2 my-8 "
                    <> "border-2 rounded border-gray-500 "
                    <> "text-gray-200 bg-gray-700 "
                    <> "hover:border-red-700 hover:bg-red-900 hover:text-red-300 "
                    <> "transition-colors hover:transition-colors "
                }
            ]
        , className: "h-full flex flex-col items-center text-center"
        }
