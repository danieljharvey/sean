module Main where

import App.Edit (toggleEdit,editUpdate)
import Prelude

import App.State (Model, Msg(..))
import App.Story (Story, parseStory, writeStory)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Hedwig ((:>))
import Simple.JSON (readJSON)

import App.View.Edit as Edit
import App.View.Story as Story
import Data.Argonaut.Core as J
import Hedwig as H
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response as AXRes

initStory :: Story
initStory = {title: "test", screens: []}

init :: Model
init = { story: initStory
       , play: {currentKey: "start"}
       , edit: {editing: false, currentIndex: Nothing}
       }

type StoryJson
  = {json :: String}

endpoint :: String
endpoint = "/"

loadStory :: Aff (Maybe Story)
loadStory = do
    res <- AX.get AXRes.json $ endpoint <> "static/story/1.json"
    pure $ parseStory $ J.stringify res.response

decodeStoryJson :: String -> Maybe Story
decodeStoryJson s = hush $ readJSON s

update :: H.Update Model Msg
update model msg = case msg of
    Reset -> model { play = init.play
                   } :> []
    DoNothing unit -> model :> []
    LogJSON -> model :> [DoNothing <$> H.sync (log $ writeStory model.story)]
    ToggleEdit -> model {edit = toggleEdit model.edit} :> []
    StartLoad -> model :> [LoadComplete <$> loadStory]
    LoadComplete a -> model {story = fromMaybe model.story a} :> []
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
