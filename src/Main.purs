module Main where

import Prelude

import App.State (Model, Msg(..))
import App.Story.Test (testStory)
import App.View.Story (renderStory)
import Effect (Effect)
import Hedwig ((:>))
import Hedwig as H

init :: Model
init = {
  story : testStory,
  state : {
    currentKey : "start"
  }
}

update :: Model -> Msg -> Model
update model = case _ of
  Reset -> init
  ChangeScreen newKey -> model { state = model.state { currentKey = newKey } }

view :: Model -> H.Html Msg
view model = H.main [H.id "main"] [
  H.div [] [H.button [H.onClick Reset] [H.text "Start over!"]],
  renderStory model.state.currentKey model.story
]

main :: Effect Unit
main = do
  H.mount "main" {
    init: init :> [],
    update: \msg model -> update msg model :> [],
    view
  }

