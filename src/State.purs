module App.State where

import App.Story (Story, Key)

import Data.Maybe

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
         | ChangeScreen Key 
         | ChangeEditScreen Key
         | EditKey Key Key
         | EditText Key String
         | EditImg Key String
         | EditAddLink Key
         | EditLinkKey Key Int String
         | EditLinkText Key Int String
