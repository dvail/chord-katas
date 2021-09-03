module Client.Components.ScaleDegrees where

import Prelude
import Data.List (List, snoc)

import Core.Data.Music
  ( ScaleDegree
  , displayScaleDegree
  , sdI
  , sdII
  , sdIII
  , sdIV
  , sdV
  , sdVI
  , sdVII
  )
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

updateSelected
  :: ScaleDegreeList
  -> ScaleDegree Int
  -> ScaleDegreeList
-- TODO Appending to the end of the list here is O(2n)
updateSelected selected clicked = snoc selected clicked

scaleDegree
  :: ((ScaleDegreeList -> ScaleDegreeList) -> Effect Unit)
  -> ScaleDegreeList
  -> ScaleDegree Int
  -> React.JSX
scaleDegree onChange selected sd =
  R.div
    { children: [ R.text $ displayScaleDegree sd ]
    , onClick: handler_ $ onChange \_ -> updateSelected selected sd
    , className: "inline-block mx-2"
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
              <$> [ sdI, sdII, sdIII, sdIV, sdV, sdVI, sdVII ]
        , className: "cursor-pointer font-serif font-bold text-2xl text-gray-300"
        }
