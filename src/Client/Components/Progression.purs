module Client.Components.Progression where

import Data.List
import Prelude

import Core.Data.Music (Note(..), ScaleDegree, displayScaleDegree, majorProgression, sdI, sdII, sdIII, sdIV, sdV, sdVI, sdVII)
import Data.Array as Array
import Data.List (snoc)
import Data.Map (update)
import Data.String (joinWith)
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

progression :: Note -> List (ScaleDegree Int) -> React.JSX
progression rootNote scaleDegrees = 
    R.div
      { children:
        [ R.text $ show $ majorProgression rootNote scaleDegrees]
      }

mkProgression :: Component Props
mkProgression = do
  component "Progression" \props -> React.do
    pure
      $ R.div
        { children:
            [ R.div
                { children:
                    [ R.text $ "Chords: " <> (joinWith " " $ Array.fromFoldable $ displayScaleDegree <$> props.scaleDegrees)
                    , progression props.rootNote props.scaleDegrees
                    ]
                }
            ]
        }
