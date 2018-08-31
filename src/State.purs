module App.State where

import App.Story (Story, Key)

type Model = {
  story :: Story,
  state :: {
    currentKey :: Key
  }
}

data Msg = Reset | ChangeScreen Key
