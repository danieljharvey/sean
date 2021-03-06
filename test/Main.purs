module Test.Main where

import Prelude

import App.Story ( Story
                 , addEmptyLink
                 , findScreen
                 , img
                 , links
                 , parseStory
                 , screens
                 , text
                 , title
                 , updateAddScreen
                 , updateImg
                 , updateKey
                 , updateLinkKey
                 , updateLinkText
                 , updateText
                 , validateLink
                 )
import Data.Array (head, length)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)

import Node.FS.Aff as FS
import Test.Unit.Assert as Assert

getTestStory :: Aff (Maybe Story)
getTestStory = do
    fileContents <- FS.readTextFile UTF8 "./test/test.json"
    pure $ parseStory fileContents

testStory :: Story
testStory = { title: "test"
            , screens: [ { key: "test"
                         , img: Nothing
                         , text: "test time"
                         , links: []
                         }
                       ]
            }

addedScreenStory :: Story
addedScreenStory = { title: "test"
                   , screens: [ { key: "test"
                                , img: Nothing
                                , text: "test time"
                                , links: []
                                }
                              , {key: "", img: Nothing, text: "", links: []}
                              ]
                   }

expectedKeyChange :: Story
expectedKeyChange = { title: "test"
                    , screens: [ { key: "changed"
                                 , img: Nothing
                                 , text: "test time"
                                 , links: []
                                 }
                               ]
                    }

expectedTextChange :: Story
expectedTextChange = { title: "test"
                     , screens: [ { key: "test"
                                  , img: Nothing
                                  , text: "bing bong"
                                  , links: []
                                  }
                                ]
                     }

expectedImgChange :: Story
expectedImgChange = { title: "test"
                    , screens: [ { key: "test"
                                 , img: Just "image.jpg"
                                 , text: "test time"
                                 , links: []
                                 }
                               ]
                    }

addEmptyLinkStory :: Story
addEmptyLinkStory = { title: "test"
                    , screens: [ { key: "test"
                                 , img: Nothing
                                 , text: "test time"
                                 , links: [{text: "", key: ""}]
                                 }
                               ]
                    }

changedLinkKeyStory :: Story
changedLinkKeyStory = { title: "test"
                      , screens: [ { key: "test"
                                   , img: Nothing
                                   , text: "test time"
                                   , links: [{text: "", key: "poo"}]
                                   }
                                 ]
                      }

changedLinkTextStory :: Story
changedLinkTextStory = { title: "test"
                       , screens: [ { key: "test"
                                    , img: Nothing
                                    , text: "test time"
                                    , links: [{text: "yep", key: ""}]
                                    }
                                  ]
                       }

main :: Effect Unit
main = runTest do
    suite "Parsing story" do
      test "Parses title" do
        story <- getTestStory
        Assert.equal (Just "Test") (title <$> story)
      test "Parses number of screens" do
        story <- getTestStory
        Assert.equal (Just 1) (length <$> screens <$> story)
      test "Finds 'start' screen" do
        story <- getTestStory
        Assert.equal true $ isJust $ findScreen "start" =<< story
      test "Does not find 'non-start' screen" do
        story <- getTestStory
        Assert.equal false $ isJust $ findScreen "non-start" =<< story
      test "Finds image on first screen screen" do
        story <- getTestStory
        Assert.equal true $ isJust $ (img <<< fst) =<< findScreen "start" =<< story
      test "Finds title of first screen" do
        story <- getTestStory
        Assert.equal (Just "Good morning Sean.") ((text <<< fst) <$> (findScreen "start" =<< story))
      test "Counts 3 links in first story" do
        story <- getTestStory
        Assert.equal (Just 3) (length <$> (links <<< fst) <$> (findScreen "start" =<< story))
      suite "Validating story" do
        test "Validates link is not valid" do
          story <- getTestStory
          let firstLink = head =<< (links <<< fst) <$> (findScreen "start" =<< story)
              
          let valid = validateLink <$> story <*> firstLink
              
          Assert.equal (Just false) valid
      suite "Editing" do
        test "Change a key" do
          Assert.equal expectedKeyChange $ updateKey 0 "changed" testStory
        test "Change the text" do
          Assert.equal expectedTextChange $ updateText 0 "bing bong" testStory
        test "Change the image" do
          Assert.equal expectedImgChange $ updateImg 0 "image.jpg" testStory
        test "Changing image to empty string makes Nothing" do
          Assert.equal testStory $ updateImg 0 "" testStory
        test "Adding an empty link" do
          Assert.equal addEmptyLinkStory $ addEmptyLink 0 testStory
        test "Change a link's key" do
          Assert.equal changedLinkKeyStory $ updateLinkKey 0 0 "poo" addEmptyLinkStory
        test "Change a link's text" do
          Assert.equal changedLinkTextStory $ updateLinkText 0 0 "yep" addEmptyLinkStory
        test "Add a new screen" do
          Assert.equal addedScreenStory $ updateAddScreen testStory
