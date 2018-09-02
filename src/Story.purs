module App.Story where

import Prelude

import Data.Array (find, head)
import Data.Either (hush)
import Data.Maybe (Maybe, isJust)
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