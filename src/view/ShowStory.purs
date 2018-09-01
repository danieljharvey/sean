module App.View.Story where

import Prelude
import App.State (Msg(..))
import App.Story (Key, Screen, Story, Link, findScreen, validateLink)
import Control.Monad.Reader (Reader, ask, runReader)
import Data.Maybe (fromMaybe)
import Hedwig as H

errorScreen :: Reader Story (H.Html Msg)
errorScreen = pure $ H.div [] [H.text "Error - could not find screen"]

renderStory :: Key -> Story -> Reader Story (H.Html Msg)
renderStory key story = fromMaybe errorScreen $ map renderScreen (findScreen key story)

renderScreen :: Screen -> Reader Story (H.Html Msg)
renderScreen scr = do
    story <- ask
    pure $ H.main [H.id ("screen" <> scr.key)] [
        H.text scr.text,
        runReader (renderLinks scr.links) story
    ]
    
renderLinks :: Array Link -> Reader Story (H.Html Msg)
renderLinks links = do
    story <- ask
    pure $ H.div [] (map (renderLink story) links)

renderLink :: Story -> Link -> H.Html Msg
renderLink story link = H.button attrs [H.text link.text]
    where attrs = case (validateLink story link) of
            true  -> [H.onClick $ ChangeScreen link.key]
            false -> []