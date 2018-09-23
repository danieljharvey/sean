module App.View.Story where

import Prelude

import App.State (Msg(..))
import App.Story (Key, Link, Screen, Story, findScreen, validateLink)
import Control.Monad.Reader (Reader, ask, runReader)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (fst)

import Hedwig as H
import Data.String (Pattern(..), split)

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
        renderImage scr.img,
        renderText scr.text,
        runReader (renderLinks scr.links) story
    ]

renderImage :: Maybe String -> H.Html Msg
renderImage image = fromMaybe default $ map showImage image
    where default = H.div [] []
          showImage = (\img -> H.img [H.src img, H.class' "storyImage"] [])

splitPattern :: Pattern
splitPattern = Pattern "\n\n"

renderText :: String -> H.Html Msg
renderText s = H.div [] paragraphs
    where paragraphs = map (\txt -> H.p [] [H.text txt]) (split splitPattern s)
    
renderLinks :: Array Link -> Reader Story (H.Html Msg)
renderLinks links = do
    story <- ask
    pure $ H.div [] (map (renderLink story) links)

renderLink :: Story -> Link -> H.Html Msg
renderLink story link = H.a attrs [H.text link.text]
    where attrs = case (validateLink story link) of
            true  -> [
                H.onClick $ ChangeScreen link.key,
                H.class' "activeStoryLink"
            ] <> fixedAttrs
            false -> [] <> fixedAttrs
          fixedAttrs = [H.class' "storyLink"]
