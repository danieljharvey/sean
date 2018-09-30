module App.Edit where

import Data.Tuple (Tuple(..))
import Prelude (bind, map, not, pure, ($), (==))
import App.State (EditMsg(..), EditSettings, Model, Msg)
import App.Story (Link, Screen, Story, addEmptyLink, updateAddScreen, updateImg, updateKey, updateLinkKey, updateLinkText, updateText)
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff (Aff)
import Hedwig ((:>))
import Data.Array (elemIndex, index)

type ScreenWithIndex
  = Tuple Screen Int

editUpdate :: Model -> EditMsg -> Tuple Model (Array (Aff Msg))
editUpdate model editMsg = case editMsg of
  AddScreen -> model {story = updateAddScreen model.story} :> []
  EditKey screenIndex newKey -> model { story = updateKey screenIndex newKey model.story
                                      } :> []
  EditText screenIndex newText -> model { story = updateText screenIndex newText model.story
                                        } :> []
  EditImg screenIndex newImg -> model { story = updateImg screenIndex newImg model.story
                                      } :> []
  EditAddLink screenIndex -> model { story = addEmptyLink screenIndex model.story
                                   } :> []
  EditLinkKey screenIndex index newLink -> model { story = updateLinkKey screenIndex index newLink model.story
                                                 } :> []
  EditLinkText screenIndex index newText -> model { story = updateLinkText screenIndex index newText model.story
                                                  } :> []
  ChangeEditScreen index -> model { edit = model.edit { currentIndex = Just index } } :> []

getEditingScreen :: EditSettings -> Story -> Maybe ScreenWithIndex
getEditingScreen edit story = do
    i <- edit.currentIndex
    screen <- index story.screens i
    pure $ Tuple screen i

isEditing :: EditSettings -> ScreenWithIndex -> Boolean
isEditing edit (Tuple screen index) = edit.currentIndex == Just index

linkIsValid :: Array Screen -> Link -> Boolean
linkIsValid screens link = isJust $ elemIndex link.key screenKeys
    where
    screenKeys = map (\t ->
        t.key) screens


toggleEdit :: EditSettings -> EditSettings
toggleEdit edit = edit {editing = (not edit.editing)}
