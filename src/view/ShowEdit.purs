module App.View.Edit where

import Prelude

import App.Edit (getEditingScreen, isEditing, linkIsValid)
import App.OnTextAreaInput (onTextAreaInput)
import App.State (EditSettings, Msg(..), EditMsg(..))
import App.Story (Link, Screen, Story)
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))

import Hedwig as H

type Index
  = Int

view :: EditSettings -> Story -> H.Html Msg
view edit story = case edit.editing of
    false -> H.div [] []
    true -> sideBar edit story

sideBar :: EditSettings -> Story -> H.Html Msg
sideBar edit story = H.div [ H.class' "sideBar"
                           ] [ H.button [H.onClick StartLoad] [H.text "LOAD"]
                             , H.button [H.onClick LogJSON] [H.text "LOG"]
                             , H.div [ H.class' "screenList"
                                     ] $ [screenList edit story]
                             , H.div [H.class' "screenForm"] $ [editForm]
                             ]
    where
    editForm = case (getEditingScreen edit story) of
        Just (Tuple scr i) -> screenForm story.screens scr i
        _ -> H.div [] [H.text "nothingness"]


screenList :: EditSettings -> Story -> H.Html Msg
screenList edit story = H.div [
                              ] [ H.p [
                                      ] [H.text "Screens"]
                                , H.button [ H.onClick $ EditAction AddScreen
                                           ] [H.text "Add screen"]
                                , H.div [
                                        ] $ mapWithIndex (screen edit) story.screens
                                ]

screen :: EditSettings -> Index -> Screen -> H.Html Msg
screen edit screenIndex scr = H.div [ H.onClick $ EditAction $ ChangeEditScreen screenIndex
                                    , H.class' editingClass
                                    , H.class' "showCircle"
                                    ] [H.text $ scr.key <> " - " <> scr.text]
    where
    editingClass = if isEditing edit $ Tuple scr screenIndex then "editingScreen" else "notEditingScreen"


screenForm :: Array Screen -> Screen -> Int -> H.Html Msg
screenForm screens scr screenIndex = H.div [ H.class' "form"
                                           ] [ H.div [
                                                     ] [ H.label [ H.for "key"
                                                                 ] [ H.text "Key:"
                                                                   ]
                                                       , H.input [ H.name "key"
                                                                 , H.type' "text"
                                                                 , H.value scr.key
                                                                 , H.onInput (\s -> EditAction $ EditKey screenIndex s)
                                                                 ] []
                                                       ]
                                             , H.div [
                                                     ] [ H.label [ H.for "text"
                                                                 ] [ H.text "Text:"
                                                                   ]
                                                       , H.textarea [ H.name "text"
                                                                    , onTextAreaInput (\s -> EditAction $ EditText screenIndex s)
                                                                    ] [ H.text scr.text
                                                                      ]
                                                       ]
                                             , H.div [
                                                     ] [ H.label [ H.for "img"
                                                                 ] [ H.text "Image:"
                                                                   ]
                                                       , H.input [ H.name "img"
                                                                 , H.value $ fromMaybe "" scr.img
                                                                 , H.onInput (\s -> EditAction $ EditImg screenIndex s)
                                                                 ] []
                                                       ]
                                             , H.div [
                                                     ] [ H.label [ H.for "links"
                                                                 ] [ H.text "Links:"
                                                                   ]
                                                       , H.div [ H.class' "links"
                                                               ] $ mapWithIndex (screenLink screens screenIndex) scr.links
                                                       , H.button [ H.onClick (EditAction $ EditAddLink screenIndex)
                                                                  ] [ H.text "Add new link"
                                                                    ]
                                                       ]
                                             ]

screenLink :: Array Screen -> Int -> Int -> Link -> H.Html Msg
screenLink screens screenIndex linkIndex link = H.div [ H.class' "screenLink"
                                                      , H.class' "showCircle"
                                                      , H.class' linkClass
                                                      ] [ H.text "Link to:"
                                                        , H.input [ H.type' "text"
                                                                  , H.value link.key
                                                                  , H.onInput (\s -> EditAction $ EditLinkKey screenIndex linkIndex s)
                                                                  ] []
                                                        , H.text "Text:"
                                                        , H.input [ H.type' "text"
                                                                  , H.value link.text
                                                                  , H.onInput $ (\s -> EditAction $ EditLinkText screenIndex linkIndex s)
                                                                  ] []
                                                        ]
    where
    linkClass = if (linkIsValid screens link) then "validLink" else "invalidLink"
