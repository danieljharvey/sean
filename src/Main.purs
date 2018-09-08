module Main where

import Prelude

import App.State (Model, Msg(..))
import App.Story.Test (testStory)
import App.Story (updateKey, updateText, updateImg, addEmptyLink, updateLinkKey, updateLinkText)
import App.View.Story as Story
import App.View.Edit as Edit
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Hedwig ((:>))
import Hedwig as H

init :: Model
init = {
  story : testStory,
  play : {
    currentKey : "start"
  },
  edit : {
    editing : true,
    currentKey : Just "start"
  }
}

update :: Model -> Msg -> Model
update model = case _ of
  Reset -> init
  ChangeScreen newKey -> model { play = model.play { currentKey = newKey } }
  ChangeEditScreen newKey -> model { edit = model.edit { currentKey = Just newKey } }
  EditKey oldKey newKey -> model { 
    edit = model.edit { currentKey = Just newKey }, 
    story = updateKey oldKey newKey model.story
  }
  EditText oldKey newText -> model {
    story = updateText oldKey newText model.story
  }
  EditImg oldKey newImg -> model {
    story = updateImg oldKey newImg model.story
  }
  EditAddLink oldKey -> model {
    story = addEmptyLink oldKey model.story
  }
  EditLinkKey oldKey index newLink -> model {
    story = updateLinkKey oldKey index newLink model.story
  }
  EditLinkText oldKey index newText -> model {
    story = updateLinkText oldKey index newText model.story
  }

view :: Model -> H.Html Msg
view model = H.main [H.id "main"] [
  H.div [H.class' "play"] [
    H.button [H.onClick Reset] [H.text "Start over!"],
    Story.view model.play.currentKey model.story
  ],
  Edit.view model.edit model.story
]

main :: Effect Unit
main = do
  H.mount "main" {
    init: init :> [],
    update: \msg model -> update msg model :> [],
    view
  }

