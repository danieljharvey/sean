module App.Story.Test where

import Data.Maybe

import App.Story (Story)

testStory :: Story
testStory = { title: "Test"
            , screens: [ { key: "start"
                         , img: Just "http://onwardolympians.com/seans-big-day/images/start.jpg"
                         , text: "Good morning Sean."
                         , links: [ {text: "Go to the toilet", key: "toilet"}
                                  , { text: "Go to the small claims court"
                                    , key: "small-claims-court"
                                    }
                                  , { text: "Eat some Special K"
                                    , key: "special-k"
                                    }
                                  ]
                         }
                       , { key: "toilet"
                         , img: Nothing
                         , text: "It is the toilet"
                         , links: []
                         }
                       ]
            }
