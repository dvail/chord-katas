module Client.Components.ScaleDegrees where

import Prelude

import Core.Data.Music (ScaleDegree, displayScaleDegree, sdI, sdII, sdIII, sdIV, sdV, sdVI, sdVII)
import Data.List (List, snoc)
import Effect (Effect)
import React.Basic.DOM (css)
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
  R.li
    { children: [ R.text $ displayScaleDegree sd ]
    , onClick: handler_ $ onChange \_ -> updateSelected selected sd
    , title: "Add chord of scale degree " <> displayScaleDegree sd
    , className: "mx-3 p-2 "
        <> "text-gray-500 hover:text-gray-300 "
        <> "bg-gray-800 hover:bg-gray-700 "
    , style: css { flexBasis: "calc(100% / 7)" }
    }

mkScaleDegrees :: Component Props
mkScaleDegrees = do
  component "ScaleDegrees" \props -> React.do
    pure
      $ R.ul
        { children:
            scaleDegree
              props.onSelectedChange
              props.selectedScaleDegrees
              <$> [ sdI, sdII, sdIII, sdIV, sdV, sdVI, sdVII ]
        , className: "cursor-pointer "
            <> "flex flex-row "
            <> "font-serif font-bold text-3xl "
            <> "my-4 w-1/3 "
        }
