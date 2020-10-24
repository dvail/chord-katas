module Client.Root where

import Prelude

import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Events (handler_)
import React.Basic.DOM as R

type Props =
  { label :: String
  }

root :: Component Int
root = do
  component "Root" \initialValue -> React.do
    counter /\ setCounter <- useState initialValue

    pure
      $ R.button
          { onClick: handler_ do
              setCounter (_ + 1)
          , children:
              [ R.text $ "Increment: " <> show counter ]
          }