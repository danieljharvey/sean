module App.View.Edit where

import Prelude

import App.Edit (getEditingScreen)
import App.State (EditSettings, Msg(..))
import App.Story (Story, Screen, Link)
import Data.Maybe (Maybe(..), fromMaybe)
import Hedwig as H

view :: EditSettings -> Story -> H.Html Msg
view edit story = case edit.editing of 
    false -> H.div [] []
    true  -> sideBar edit story

sideBar :: EditSettings -> Story -> H.Html Msg
sideBar edit story = H.div [H.class' "sideBar"] [
    H.div [H.class' "screenList"] $ [screenList story],
    H.div [H.class' "screenForm"] $ [editForm]
] where editForm = case (getEditingScreen edit story) of 
            Just scr -> screenForm scr
            _        -> H.div [] [H.text "nothingness"]

screenList :: Story -> H.Html Msg
screenList story = H.div [] [
    H.p [] [H.text "Screens"],
    H.div [] $ map screen story.screens
]

screen :: Screen -> H.Html Msg
screen scr = H.div [H.onClick $ ChangeEditScreen scr.key] [H.text $ scr.key <> " - " <> scr.text]

screenForm :: Screen -> H.Html Msg
screenForm scr = H.form [] [
    H.div [] [
        H.label [H.for "key"] [H.text "Key:"],
        H.input [
            H.name "key",
            H.type' "text",
            H.value scr.key,
            H.onInput $ EditKey scr.key
        ] []
    ],
    H.div [] [
        H.label [H.for "text"] [H.text "Text:"],
        H.input [
            H.name "text",
            H.type' "text",
            H.value scr.text,
            H.onInput $ EditText scr.key
        ] []
    ],
    H.div [] [
        H.label [H.for "img"] [H.text "Image:"],
        H.input [
            H.name "img",
            H.value $ fromMaybe "" scr.img,
            H.onInput $ EditImg scr.key
        ] []
    ],
    H.div [] [
        H.label [H.for "links"] [H.text "Links:"],
        H.div [H.class' "links"] $ map screenLink scr.links
    ]
]

screenLink :: Link -> H.Html Msg
screenLink link = H.div [H.class' "screenLink"] [H.text $ link.key <> " - " <> link.text]