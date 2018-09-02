module App.State where

import App.Story (Story, Key)

type EditSettings = {
  editing :: Boolean
}

type Model = {
  story :: Story,
  play :: {
    currentKey :: Key
  },
  edit :: EditSettings
}

data Msg = Reset | ChangeScreen Key
