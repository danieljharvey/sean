module App.Story where

import Prelude

import Data.Array (find, head, snoc, mapWithIndex)
import Data.Either (hush)
import Data.Maybe (Maybe(..), isJust)
import Data.String (length)
import Simple.JSON (readJSON)

type Key = String

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

parseStory :: String -> Maybe Story
parseStory s = hush $ readJSON s

title :: Story -> String
title s = s.title

screens :: Story -> Array Screen
screens s = s.screens

findScreen :: Key -> Story -> Maybe Screen
findScreen k s = find (\scr -> scr.key == k) s.screens

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

updateKey :: Key -> Key -> Story -> Story
updateKey oldKey newKey story = story { screens = newScreens }
    where newScreens = map (\scr -> if scr.key == oldKey then scr { key = newKey } else scr) story.screens

updateText :: Key -> String -> Story -> Story
updateText oldKey newText story = story { screens = newScreens }
    where newScreens = map (\scr -> if scr.key == oldKey then scr { text = newText } else scr) story.screens

updateImg :: Key -> String -> Story -> Story
updateImg oldKey newImg story = story { screens = newScreens }
    where newScreens = map (\scr -> if scr.key == oldKey then (putImageInScreen newImg scr) else scr) story.screens

putImageInScreen :: String -> Screen -> Screen
putImageInScreen newImg scr = case (length newImg > 0) of
    true -> scr { img = Just newImg }
    false -> scr { img = Nothing }

addEmptyLink :: Key -> Story -> Story
addEmptyLink oldKey story = story { screens = newScreens }
    where newScreens = map (\scr -> if scr.key == oldKey then scr { links = addEmptyLinkToArray scr.links } else scr) story.screens

addEmptyLinkToArray :: Array Link -> Array Link
addEmptyLinkToArray linkArray = snoc linkArray { text: "", key: "" }

updateLinkKey :: Key -> Int -> String -> Story -> Story
updateLinkKey oldKey index newLink story = story { screens = newScreens }
    where newScreens = map (\scr -> if scr.key == oldKey then scr { links = updateLinkByKey index newLink scr.links } else scr) story.screens

updateLinkByKey :: Int -> String -> Array Link -> Array Link
updateLinkByKey index newLink linkArray = mapWithIndex (\i -> \link -> if i == index then link { key = newLink } else link) linkArray

updateLinkText:: Key -> Int -> String -> Story -> Story
updateLinkText oldKey index newText story = story { screens = newScreens }
    where newScreens = map (\scr -> if scr.key == oldKey then scr { links = updateLinkTextByKey index newText scr.links } else scr) story.screens

updateLinkTextByKey :: Int -> String -> Array Link -> Array Link
updateLinkTextByKey index newText linkArray = mapWithIndex (\i -> \link -> if i == index then link { text = newText } else link) linkArray
