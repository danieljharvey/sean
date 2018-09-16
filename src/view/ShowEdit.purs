module App.View.Edit where

import Prelude

import App.Edit (getEditingScreen, isEditing, linkIsValid)
import App.OnTextAreaInput (onTextAreaInput)
import App.State (EditSettings, Msg(..))
import App.Story (Story, Screen, Link, Key)
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Hedwig as H

view :: EditSettings -> Story -> H.Html Msg
view edit story = case edit.editing of 
    false -> H.div [] []
    true  -> sideBar edit story

sideBar :: EditSettings -> Story -> H.Html Msg
sideBar edit story = H.div [H.class' "sideBar"] [
    H.button [H.onClick StartLoad] [H.text "LOAD"],
    H.button [H.onClick LogJSON] [H.text "LOG"],
    H.div [H.class' "screenList"] $ [screenList edit story],
    H.div [H.class' "screenForm"] $ [editForm]
] where editForm = case (getEditingScreen edit story) of 
            Just scr -> screenForm story.screens scr
            _        -> H.div [] [H.text "nothingness"]

screenList :: EditSettings -> Story -> H.Html Msg
screenList edit story = H.div [] [
    H.p [] [H.text "Screens"],
    H.button [H.onClick AddScreen] [H.text "Add screen"],
    H.div [] $ map (screen edit) story.screens
]

screen :: EditSettings -> Screen -> H.Html Msg
screen edit scr = H.div [
        H.onClick $ ChangeEditScreen scr.key,
        H.class' editingClass,
        H.class' "showCircle" 
    ] [
        H.text $ scr.key <> " - " <> scr.text
    ]
    where editingClass = if isEditing edit scr then "editingScreen" else "notEditingScreen"

screenForm :: Array Screen -> Screen -> H.Html Msg
screenForm screens scr = H.div [H.class' "form"] [
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
        H.textarea [
            H.name "text",
            onTextAreaInput $ EditText scr.key
        ] [H.text scr.text]
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
        H.div [H.class' "links"] $ mapWithIndex (screenLink screens scr.key) scr.links,
        H.button [H.onClick $ EditAddLink scr.key] [H.text "Add new link"]
    ]
]

screenLink :: Array Screen -> Key ->  Int ->  Link -> H.Html Msg
screenLink screens key index link = H.div [H.class' "screenLink", H.class' "showCircle", H.class' linkClass] [
    H.text "Link to:",
    H.input [
        H.type' "text",
        H.value link.key,
        H.onInput $ EditLinkKey key index 
    ] [],
        H.text "Text:",
    H.input [
        H.type' "text",
        H.value link.text,
        H.onInput $ EditLinkText key index 
    ] []
]
    where linkClass = if (linkIsValid screens link) then "validLink" else "invalidLink"