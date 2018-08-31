module App.View.Story where

import Prelude

import App.State (Msg(..))
import App.Story (Key, Screen, Story, Link, findScreen)
import Data.Maybe (fromMaybe)
import Hedwig as H

errorScreen :: H.Html Msg
errorScreen = H.div [] [H.text "Error - could not find screen"]

renderStory :: Key -> Story -> H.Html Msg
renderStory key story = fromMaybe errorScreen $ map renderScreen (findScreen key story)

renderScreen :: Screen -> H.Html Msg
renderScreen scr = H.main [H.id ("screen" <> scr.key)] [
    H.text scr.text,
    renderLinks scr.links
]

renderLinks :: Array Link -> H.Html Msg
renderLinks links = H.div [] (map renderLink links)

renderLink :: Link -> H.Html Msg
renderLink link = H.button [H.onClick $ ChangeScreen link.key] [H.text link.text]