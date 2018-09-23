module App.Edit where

import Prelude
import App.State (EditSettings)
import App.Story (Story, Screen, Link, findScreen)
import Data.Array (elemIndex, index)
import Data.Maybe (Maybe(..), isJust)

import Data.Tuple

type ScreenWithIndex = Tuple Screen Int

getEditingScreen :: EditSettings -> Story -> Maybe ScreenWithIndex
getEditingScreen edit story = do
    i <- edit.currentIndex
    screen <- index story.screens i
    pure $ Tuple screen i

isEditing :: EditSettings -> ScreenWithIndex -> Boolean
isEditing edit (Tuple screen index) = edit.currentIndex == Just index

linkIsValid :: Array Screen -> Link -> Boolean
linkIsValid screens link = isJust $ elemIndex link.key screenKeys
    where screenKeys = map (\t -> t.key) screens
