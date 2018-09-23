module App.Story where

import Prelude
import Data.Array (findIndex, head, index, mapWithIndex, modifyAt, snoc)
import Data.Either (hush)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.String (length)
import Simple.JSON (readJSON, writeJSON)
import Type.Data.Boolean (kind Boolean)

type Key = String
type Index = Int

type Link = {
    text :: String,
    key :: Key
}

type Screen = {
    key :: Key,
    img :: Maybe String,
    text :: String,
    links :: Array Link
}

type Story =
  { title :: String
  , screens :: Array Screen
  }

emptyScreen :: Screen
emptyScreen = {
    key: "",
    img: Nothing,
    text: "",
    links: []
}

parseStory :: String -> Maybe Story
parseStory s = hush $ readJSON s

writeStory :: Story -> String
writeStory = writeJSON

title :: Story -> String
title s = s.title

screens :: Story -> Array Screen
screens s = s.screens

type ScreenWithIndex = Tuple Screen Int

findScreen :: Key -> Story -> Maybe ScreenWithIndex
findScreen k s = do
    i <- findIndex (\scr -> scr.key == k) s.screens
    item <- index s.screens i
    pure $ Tuple item i

firstScreen :: Story -> Maybe Screen
firstScreen s = head $ screens s

key :: Screen -> Key
key s = s.key

img :: Screen -> Maybe String
img s = s.img

text :: Screen -> String
text s = s.text

links :: Screen -> Array Link
links s = s.links

validateLink :: Story -> Link -> Boolean
validateLink s l = isJust $ findScreen l.key s

weirdValid :: Maybe Story -> Maybe Link -> Maybe Boolean
weirdValid ms ml = validateLink <$> ms <*> ml

updateKey :: Index -> Key -> Story -> Story
updateKey index newKey story = story { screens = newScreens }
    where newScreens = fromMaybe [] $ modifyAt index (\scr -> scr { key = newKey }) story.screens

updateText :: Index -> String -> Story -> Story
updateText index newText story = story { screens = newScreens }
    where newScreens = fromMaybe [] $ modifyAt index (\scr -> scr { text = newText }) story.screens

updateImg :: Index -> String -> Story -> Story
updateImg index newImg story = story { screens = newScreens }
    where newScreens = fromMaybe [] $ modifyAt index (\scr -> putImageInScreen newImg scr) story.screens

putImageInScreen :: String -> Screen -> Screen
putImageInScreen newImg scr = case (length newImg > 0) of
    true -> scr { img = Just newImg }
    false -> scr { img = Nothing }

addEmptyLink :: Index -> Story -> Story
addEmptyLink index story = story { screens = newScreens }
    where newScreens = fromMaybe [] $ modifyAt index (\scr -> scr { links = addEmptyLinkToArray scr.links } ) story.screens

addEmptyLinkToArray :: Array Link -> Array Link
addEmptyLinkToArray linkArray = snoc linkArray { text: "", key: "" }

updateLinkKey :: Index -> Index -> String -> Story -> Story
updateLinkKey screenIndex index newLink story = story { screens = newScreens }
    where newScreens = fromMaybe [] $ modifyAt screenIndex (\scr -> scr { links = updateLinkByKey index newLink scr.links }) story.screens

updateLinkByKey :: Index -> String -> Array Link -> Array Link
updateLinkByKey index newLink linkArray = mapWithIndex (\i -> \link -> if i == index then link { key = newLink } else link) linkArray

updateLinkText:: Index -> Index -> String -> Story -> Story
updateLinkText screenIndex index newText story = story { screens = newScreens }
    where newScreens = fromMaybe [] $ modifyAt screenIndex (\scr -> scr { links = updateLinkTextByKey index newText scr.links }) story.screens

updateLinkTextByKey :: Index -> String -> Array Link -> Array Link
updateLinkTextByKey index newText linkArray = mapWithIndex (\i -> \link -> if i == index then link { text = newText } else link) linkArray

updateAddScreen :: Story -> Story
updateAddScreen story = story { screens = newScreens }
    where newScreens = story.screens <> [emptyScreen]
