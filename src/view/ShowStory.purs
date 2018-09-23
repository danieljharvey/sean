module App.View.Story where

import Prelude
import App.State (Msg(..))
import App.Story (Key, Screen, Story, Link, findScreen, validateLink)
import Control.Monad.Reader (Reader, ask, runReader)
import Data.Maybe (fromMaybe)
import Data.Tuple (fst)
import Hedwig as H

view :: Key -> Story -> H.Html Msg
view key story = runReader (renderStory key story) story

renderStory :: Key -> Story -> Reader Story (H.Html Msg)
renderStory key story = fromMaybe errorScreen $ map (renderScreen <<< fst) (findScreen key story)

errorScreen :: Reader Story (H.Html Msg)
errorScreen = pure $ H.div [] [H.text "Error - could not find screen"]

renderScreen :: Screen -> Reader Story (H.Html Msg)
renderScreen scr = do
    story <- ask
    pure $ H.div [H.id ("screen" <> scr.key), H.class' "story"] [
        fromMaybe (H.div [] []) $ map (\img -> H.img [H.src img] []) scr.img,
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
