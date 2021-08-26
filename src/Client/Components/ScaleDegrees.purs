module Client.Components.ScaleDegrees where

import Data.List
import Prelude

import Core.Data.Music (Note(..), ScaleDegree, displayScaleDegree, majorProgression, sd_I, sd_II, sd_III, sd_IV, sd_V, sd_VI, sd_VII)
import Data.List (snoc)
import Data.Map (update)
import Debug (trace)
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React

type ScaleDegreeList = List (ScaleDegree Int)

type Props =
  { selectedScaleDegrees :: ScaleDegreeList
  , onSelectedChange :: (ScaleDegreeList -> ScaleDegreeList) -> Effect Unit
  }

updateSelected :: 
  ScaleDegreeList ->
  ScaleDegree Int ->
  ScaleDegreeList
-- TODO Appending to the end of the list here is O(2n)
updateSelected selected clicked = snoc selected clicked

scaleDegree :: 
  ((ScaleDegreeList -> ScaleDegreeList) -> Effect Unit) ->
  ScaleDegreeList ->
  ScaleDegree Int ->
  React.JSX
scaleDegree onChange selected sd = 
    R.div
      { children: [R.text $ displayScaleDegree sd]
      , onClick: handler_ do
          onChange $ \_ -> updateSelected selected sd
      }

mkScaleDegrees :: Component Props
mkScaleDegrees = do
  component "ScaleDegrees" \props -> React.do
    pure
      $ R.div
          { children:
              scaleDegree 
                props.onSelectedChange
                props.selectedScaleDegrees
                 <$> [sd_I, sd_II, sd_III, sd_IV, sd_V, sd_VI, sd_VII]
          , className: "text-red-800"
          }