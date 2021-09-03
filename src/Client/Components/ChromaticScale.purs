module Client.Components.ChromaticScale where

import Prelude

import Client.Style (noteBackgroundClass, noteBorderClass, noteColorClass)
import Core.Data.Music (Note, chromaticScale, displayNote)
import Effect (Effect)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React

type Props =
  { currentNote :: Note
  , onCurrentNoteChange :: (Note -> Note) -> Effect Unit
  }

clickableNote :: Props -> Note -> React.JSX
clickableNote props note =
  R.li
    { children: [ R.span_ [ R.text $ displayNote note ] ]
    , onClick: handler_ $ props.onCurrentNoteChange \_ -> note
    , title: "Switch to key of " <> displayNote note
    , className: "cursor-pointer p-2 mx-2 "
        <> "flex justify-center "
        <> "font-bold text-xl "
        <> noteColorClass note
        <> noteBackgroundClass note
        <> "border-2 rounded " <> noteBorderClass note
        <> "transition-opacity " <> elemOpacity
    , style: css { flexBasis: "calc(100% / 12)" }
    }
  where
  elemOpacity =
    if note == props.currentNote then "opacity-100 "
    else "opacity-30 hover:opacity-60"

mkChromaticScale :: Component Props
mkChromaticScale = do
  component "ChomaticScale" \props -> React.do
    pure
      $ R.ul
        { children: clickableNote props <$> chromaticScale
        , className: "my-4 w-1/2 flex flex-row justify-center "
        }