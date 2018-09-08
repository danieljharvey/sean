module Main where

import Prelude
import App.State (Model, Msg(..))
import App.Story.Test (testStory)
import App.Story (Story, updateKey, updateText, updateImg, addEmptyLink, updateLinkKey, updateLinkText, writeStory)
import App.View.Story as Story
import App.View.Edit as Edit
import Data.Maybe (Maybe(..))
import Data.Either (hush)
import Effect.Aff (launchAff_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Hedwig ((:>))
import Hedwig as H

import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response as AXRes

import Data.Argonaut.Core as J
import Simple.JSON (readJSON)

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

type StoryJson = {
  json :: String
}

endpoint :: String
endpoint = "http://localhost:8080/"

loadStory :: Effect Unit
loadStory = launchAff_ $ do
  res <- AX.get AXRes.json $ endpoint <> "stories"
  liftEffect $ log $ J.stringify res.response

decodeStoryJson :: String -> Maybe Story
decodeStoryJson s = hush $ readJSON s

update :: H.Update Model Msg
update model msg = case msg of
  Reset -> init :> []
  DoNothing unit -> model :> []
  LogJSON -> model  :> [DoNothing <$> H.sync (log $ writeStory model.story)]
  StartSave -> model :> [DoNothing <$> H.sync loadStory]
  SaveComplete a -> model :> []
  ChangeScreen newKey -> model { play = model.play { currentKey = newKey } } :> []
  ChangeEditScreen newKey -> model { edit = model.edit { currentKey = Just newKey } } :> []
  EditKey oldKey newKey -> model { 
    edit = model.edit { currentKey = Just newKey }, 
    story = updateKey oldKey newKey model.story
  } :> []
  EditText oldKey newText -> model {
    story = updateText oldKey newText model.story
  } :> []
  EditImg oldKey newImg -> model {
    story = updateImg oldKey newImg model.story
  } :> []
  EditAddLink oldKey -> model {
    story = addEmptyLink oldKey model.story
  } :> []
  EditLinkKey oldKey index newLink -> model {
    story = updateLinkKey oldKey index newLink model.story
  } :> []
  EditLinkText oldKey index newText -> model {
    story = updateLinkText oldKey index newText model.story
  } :> []

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
    update: \msg model -> update msg model,
    view
  }

