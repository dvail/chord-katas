module Client.ProgressionGenerator where

import Prelude

import Client.Style (noteBackgroundClass, noteBorderClass, noteColorClass)
import Core.Data.Music (Chord, Note(..), ScaleDegree, chordRoot, chromaticScale, displayChord, displayNote, displayScaleDegree, majorProgression, sdI, sdII, sdIII, sdIV, sdV, sdVI, sdVII)
import Data.Array as Array
import Data.List (List(..), snoc, zip)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Elmish (ComponentDef, Dispatch, ReactElement, Transition)
import Elmish.HTML.Internal (css)
import Elmish.HTML.Styled as H

type ScaleDegreeList = List (ScaleDegree Int)

data Message
  = ChangeRootNote Note
  | AddScaleDegree (ScaleDegree Int)
  | ClearProgression

type State =
  { rootNote :: Note
  , scaleDegrees :: ScaleDegreeList
  }

def :: forall m. MonadAff m => ComponentDef m Message State
def = { init, update, view }

init :: forall m. MonadAff m => Transition m Message State
init = do
  pure { rootNote: C, scaleDegrees: Nil }

update :: forall m. MonadAff m => State -> Message -> Transition m Message State
update state (ChangeRootNote note) = pure $ state { rootNote = note }
update state (AddScaleDegree sd) =
  pure $ state { scaleDegrees = snoc state.scaleDegrees sd }
update state ClearProgression = pure $ state { scaleDegrees = Nil }

view :: State -> Dispatch Message -> ReactElement
view state dispatch =
  H.div "h-full flex flex-col items-center text-center"
    [ H.h1 "text-7xl font-display mt-6 text-gray-300" "Chord Katas"
    , H.h2 "text-lg mb-8 text-gray-200" "Chord Progression Generator"
    , chromaticScalePicker
    , scaleDegreesPicker
    , H.div "flex flex-col flex-1 justify-center " [ progression ]
    , clearButton
    ]
  where

  chromaticScalePicker :: ReactElement
  chromaticScalePicker =
    H.ul "my-4 w-1/2 flex flex-row justify-center " $ clickableNote <$> chromaticScale

  clickableNote :: Note -> ReactElement
  clickableNote note =
    H.li_
      ( "cursor-pointer p-2 mx-2 "
          <> "flex justify-center "
          <> "font-bold text-xl "
          <> noteColorClass note
          <> noteBackgroundClass note
          <> noteBorderClass note
          <> "border-2 rounded "
          <> "transition-opacity "
          <> if note == state.rootNote then "opacity-100 " else "opacity-30 hover:opacity-60"
      )
      { onClick: dispatch $ ChangeRootNote note
      , title: "Switch to key of " <> displayNote note
      , style: css { flexBasis: "calc(100% / 12)" }
      }
      $ displayNote note

  scaleDegreesPicker :: ReactElement
  scaleDegreesPicker =
    H.ul
      "cursor-pointer flex flex-row font-serif font-bold text-3xl my-4 w-1/3 "
      $ scaleDegree <$> [ sdI, sdII, sdIII, sdIV, sdV, sdVI, sdVII ]

  scaleDegree :: ScaleDegree Int -> ReactElement
  scaleDegree sd =
    H.li_
       "mx-3 p-2 text-gray-500 hover:text-gray-300 bg-gray-800 hover:bg-gray-700 "
      { title: "Add chord of scale degree " <> displayScaleDegree sd
      , onClick: dispatch $ AddScaleDegree sd
      , style: css { flexBasis: "calc(100% / 7)" }
      }
      $ displayScaleDegree sd

  progression :: ReactElement
  progression =
    H.div ""
      [ H.h2 "text-xl"
          [ H.text "Progression in the Key of "
          , H.span ("font-bold " <> noteColorClass state.rootNote) $ [ H.text $ displayNote state.rootNote ]
          ]

      , case state.scaleDegrees of
          Nil -> noProgression
          _ -> selectedProgression
      ]

  noProgression :: ReactElement
  noProgression = H.h3
    "p-6 m-4 text-gray-500 text-lg rounded bg-white bg-opacity-10"
    "No progression selected"

  selectedProgression :: ReactElement
  selectedProgression =
    H.div
      "flex flex-row justify-center m-4"
      $ Array.fromFoldable
      $ sdWithChord <$> zipChords chords state.scaleDegrees
    where
    chords = majorProgression state.rootNote state.scaleDegrees
    zipChords
      :: Maybe (List Chord)
      -> List (ScaleDegree Int)
      -> List (Tuple Chord (ScaleDegree Int))
    zipChords Nothing _ = Nil
    zipChords (Just cs) sds = zip cs sds

  sdWithChord :: Tuple Chord (ScaleDegree Int) -> ReactElement
  sdWithChord (Tuple chord sd) = H.div
    ( "flex flex-col text-3xl p-4 mr-1 "
        <> "font-bold "
        <> "bg-white bg-opacity-10 "
        <> (noteColorClass <<< chordRoot) chord
    )
    [ H.span "font-serif text-gray-200 " $ H.text $ displayScaleDegree sd
    , H.text $ displayChord chord
    ]

  clearButton :: ReactElement
  clearButton =
    H.button_
      ( "px-4 py-2 my-8 "
          <> "border-2 rounded border-gray-500 "
          <> "text-gray-200 bg-gray-700 "
          <> "hover:border-red-700 hover:bg-red-900 hover:text-red-300 "
          <> "transition-colors hover:transition-colors "
      )
      { onClick: dispatch ClearProgression }
      $ "Clear All"
