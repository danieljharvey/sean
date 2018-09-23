module App.State where

import App.Story (Story, Key)
import Prelude
import Data.Maybe (Maybe)

type EditSettings = {
  editing :: Boolean,
  currentKey :: Maybe Key
}

type Model = {
  story :: Story,
  play :: {
    currentKey :: Key
  },
  edit :: EditSettings
}

type Index = Int

data Msg = Reset
         | StartLoad
         | LoadComplete (Maybe Story)
         | DoNothing Unit
         | LogJSON
         | ChangeScreen Key
         | ChangeEditScreen Key
         | AddScreen
         | EditKey Index Key
         | EditText Index String
         | EditImg Index String
         | EditAddLink Index
         | EditLinkKey Index Int String
         | EditLinkText Index Int String
