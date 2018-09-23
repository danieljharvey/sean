module App.State where

import Prelude

import App.Story (Key, Story)
import Data.Maybe (Maybe)

type EditSettings
  = {editing :: Boolean, currentIndex :: Maybe Index}

type Model
  = {story :: Story, play :: {currentKey :: Key}, edit :: EditSettings}

type Index
  = Int

data Msg
  = Reset
  | StartLoad
  | LoadComplete (Maybe Story)
  | DoNothing Unit
  | LogJSON
  | ChangeScreen Key
  | ChangeEditScreen Index
  | AddScreen
  | EditKey Index Key
  | EditText Index String
  | EditImg Index String
  | EditAddLink Index
  | EditLinkKey Index Int String
  | EditLinkText Index Int String
