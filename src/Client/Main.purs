module Client.Main where

import Prelude

import Data.List
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

import Core.Data.Music (Note(..), rn_I, rn_IV, rn_V, rn_VI)
import Client.Root (root)

main :: Effect Unit
main = do
  container <- getElementById "app" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> throw "Container element not found."
    Just c  -> do
      app <- root
      render (app { rootNote: G, chordNums: (rn_I:rn_VI:rn_V:rn_IV:Nil) }) c
