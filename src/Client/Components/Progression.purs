module Client.Components.Progression where

import Data.List
import Prelude

import Client.Components.ScaleDegrees (scaleDegree)
import Core.Data.Music (Chord, Note(..), ScaleDegree, displayChord, displayScaleDegree, majorProgression, sdI, sdII, sdIII, sdIV, sdV, sdVI, sdVII)
import Data.Array as Array
import Data.List (snoc)
import Data.Map (update)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Debug (trace)
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React

type Props =
  { rootNote :: Note
  , scaleDegrees :: List (ScaleDegree Int)
  }

sdWithChord :: Tuple Chord (ScaleDegree Int) -> React.JSX
sdWithChord (Tuple chord sd) = R.div
  { children:
      [ R.span { children: [ R.text $ displayScaleDegree sd ] }
      , R.span { children: [ R.text $ displayChord chord ] }
      ]
  , className: "flex flex-col"
  }

progression :: Note -> List (ScaleDegree Int) -> React.JSX
progression rootNote scaleDegrees =
  R.div
    { children: Array.fromFoldable $ sdWithChord <$> zipChords chords scaleDegrees
    , className: "flex flex-row"
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
      $ R.div { children: [ progression props.rootNote props.scaleDegrees ] }
