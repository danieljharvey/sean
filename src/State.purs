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


data Msg = Reset
         | StartLoad
         | ToggleEdit
         | LoadComplete (Maybe Story)
         | DoNothing Unit
         | LogJSON
         | ChangeScreen Key 
         | ChangeEditScreen Key
         | AddScreen
         | EditKey Key Key
         | EditText Key String
         | EditImg Key String
         | EditAddLink Key
         | EditLinkKey Key Int String
         | EditLinkText Key Int String
