module App.View.Edit where

import App.State (EditSettings, Msg)
import App.Story (Story)
import Hedwig as H

view :: EditSettings -> Story -> H.Html Msg
view edit story = case edit.editing of 
    false -> H.div [] []
    true  -> H.div [] []

    