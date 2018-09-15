module App.Edit where

import Prelude
import App.State (EditSettings)
import App.Story (Story, Screen, findScreen)

import Data.Maybe (Maybe(..))

getEditingScreen :: EditSettings -> Story -> Maybe Screen
getEditingScreen edit story = bind edit.currentKey (\key -> findScreen key story)

isEditing :: EditSettings -> Screen -> Boolean
isEditing edit screen = edit.currentKey == Just screen.key