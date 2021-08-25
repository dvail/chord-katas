module Client.Components.ChromaticScale where

import Prelude

import Core.Data.Music (Note, chromaticScale)
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React

type Props =
  { currentNote :: Note
  , onCurrentNoteChange :: (Note -> Note) -> Effect Unit
  }

clickableNote :: ((Note -> Note) -> Effect Unit) -> Note -> React.JSX
clickableNote onClick note = 
  R.div
    { children: [ R.text $ show note ]
    , onClick: handler_ do
        onClick \_ -> note
    , className: "mx-4 inline-block text-indigo-600"
    }


mkChromaticScale :: Component Props
mkChromaticScale = do
  component "ChomaticScale" \props -> React.do
    pure
      $ R.div
          { children:
              [ R.div { children: [R.text $ show props.currentNote] }
              ] <> (clickableNote props.onCurrentNoteChange <$> chromaticScale)
          , className: "text-red-800"
          }