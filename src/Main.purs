module Main where

import Effect (Effect)
import Effect.Class (liftEffect)
import Prelude

import App.Edit (toggleEdit, editUpdate)
import App.State (Model, Msg(..))
import App.Story (Story, parseStoryWithJson, writeStory, StoryJson)
import App.View.Edit as Edit
import App.View.Story as Story
import Data.Argonaut.Core as J
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import Effect.Console (log)
import Hedwig ((:>))
import Hedwig as H
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Request as AXReq
import Network.HTTP.Affjax.Response as AXRes
import Simple.JSON (readJSON, writeJSON)

initStory :: Story
initStory = {title: "test", screens: []}

init :: Model
init = { story: initStory
       , play: {currentKey: "start"}
       , edit: {editing: false, currentIndex: Nothing}
       }

endpoint :: String
endpoint = "/"

remotePath :: String
remotePath = "http://localhost:8080/"

localPath :: String
localPath = endpoint <> "static/story/1.json"

loadStory :: Aff (Maybe Story)
loadStory = do
    res <- AX.get AXRes.json $ remotePath <> "stories/1"
    pure $ parseStoryWithJson $ J.stringify res.response

makeStoryJson :: Story -> String
makeStoryJson story = writeJSON storyJson
    where storyJson = { json : writeJSON story } :: StoryJson

saveStory :: Story -> Aff Unit
saveStory story = do
    res2 <- AX.post AXRes.json "http://localhost:8080/stories" (AXReq.string $ makeStoryJson story)
    liftEffect $ log $ "POST /api response: " <> J.stringify res2.response

decodeStoryJson :: String -> Maybe Story
decodeStoryJson s = hush $ readJSON s

update :: H.Update Model Msg
update model msg = case msg of
    Reset -> model { play = init.play } :> []
    DoNothing unit -> model :> []
    LogJSON -> model :> [DoNothing <$> H.sync (log $ writeStory model.story)]
    ToggleEdit -> model {edit = toggleEdit model.edit} :> []
    StartLoad -> model :> [LoadComplete <$> loadStory]
    LoadComplete a -> model {story = fromMaybe model.story a} :> []
    StartSave -> model :> [DoNothing <$> saveStory model.story]
    ChangeScreen newKey -> model {play = model.play {currentKey = newKey}} :> []
    EditAction editMsg -> editUpdate model editMsg

view :: Model -> H.Html Msg
view model = H.main [ H.id "main"
                    ] [ H.div [ H.class' "play"
                              ] [ H.button [ H.onClick Reset
                                           ] [H.text "Start over!"]
                                , H.button [ H.onClick ToggleEdit
                                           ] [H.text "Toggle edit"]
                                , Story.view model.play.currentKey model.story
                                ]
                      , Edit.view model.edit model.story
                      ]

main :: Effect Unit
main = do
    H.mount "main" { init: init :> [pure StartLoad]
                   , update: \msg ->
                     \model ->
                       update msg model
                   , view
                   }
