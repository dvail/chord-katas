module Client.Root where

import Data.Array as Array
import Data.List
import Data.String
import Prelude

import Core.Data.Music 
  ( Note
  , RomanNumeral
  , majorProgression
  , rn_I
  , rn_II
  , rn_III
  , rn_IV
  , rn_V
  , rn_VI
  , rn_VII
  )
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React
import Web.DOM.Element (className)

type Props =
  { rootNote :: Note
  , chordNums :: List (RomanNumeral Int)
  }

progression :: Props -> React.JSX
progression { rootNote, chordNums } = 
    R.div
      { children:
        [ R.text $ show $ majorProgression rootNote chordNums]
      }

displayChordNum :: RomanNumeral Int -> String
displayChordNum x
  | x == rn_I = "I"
  | x == rn_II = "II"
  | x == rn_III = "III"
  | x == rn_IV = "IV"
  | x == rn_V = "V"
  | x == rn_VI = "VI"
  | x == rn_VII = "VII"
  | otherwise = ""

root :: Component Props
root = do
  component "Root" \initialValue -> React.do
    rootNote /\ setCounter <- useState initialValue.rootNote
    chordNums /\ setCounter <- useState initialValue.chordNums

    pure
      $ R.div
          { children:
              [ R.div
                  { children: 
                    [ R.div { children: [ R.text $ "Root: " <> show rootNote ] }
                    , R.div { children: [ R.text $ "Chords: " <> (joinWith " " $ Array.fromFoldable $ displayChordNum <$> chordNums) ] }
                    ]
                  }
              , progression initialValue
              ]
          , className: "text-red-500"
          }