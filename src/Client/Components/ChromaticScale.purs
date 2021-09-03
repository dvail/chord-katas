module Client.Components.ChromaticScale where

import Prelude

import Client.Util (noteBackgroundClass, noteBorderClass, noteColorClass)
import Core.Data.Music (Note, chromaticScale, displayNote)
import Effect (Effect)
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
    { children: [ R.text $ displayNote note ]
    , onClick: handler_ $ props.onCurrentNoteChange \_ -> note
    , className: "cursor-pointer w-8 h-8 mx-2 inline-block "
        <> "font-bold text-xl "
        <> noteColorClass note
        <> noteBackgroundClass note
        <> "border-2 rounded "
        <> noteBorderClass note
        <> "transition-opacity "
        <> elemOpacity
    }
  where
  elemOpacity =
    if note == props.currentNote then "opacity-100 "
    else "opacity-40 hover:opacity-60"

mkChromaticScale :: Component Props
mkChromaticScale = do
  component "ChomaticScale" \props -> React.do
    pure
      $ R.ul_
      $ clickableNote props <$> chromaticScale