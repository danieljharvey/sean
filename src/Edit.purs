module App.Edit where

import Prelude
import App.State (EditSettings)
import App.Story (Story, Screen, Link, findScreen)
import Data.Array (elemIndex)
import Data.Maybe (Maybe(..), isJust)

import Data.Tuple (Tuple)

type ScreenWithIndex = Tuple Screen Int

getEditingScreen :: EditSettings -> Story -> Maybe ScreenWithIndex
getEditingScreen edit story = bind edit.currentKey (\key -> findScreen key story)

isEditing :: EditSettings -> Screen -> Boolean
isEditing edit screen = edit.currentKey == Just screen.key

linkIsValid :: Array Screen -> Link -> Boolean
linkIsValid screens link = isJust $ elemIndex link.key screenKeys
    where screenKeys = map (\t -> t.key) screens
