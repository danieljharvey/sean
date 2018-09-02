module Test.Main where

import Prelude

import App.Story (Story, findScreen, img, links, parseStory, screens, text, title, validateLink, updateKey, updateText)
import Data.Array (length, head)
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

getTestStory :: Aff (Maybe Story)
getTestStory = do
  fileContents <- FS.readTextFile UTF8 "./test/test.json"
  pure $ parseStory fileContents

testStory :: Story
testStory = {
  title : "test",
  screens: [
    {
      key : "test",
      img : Nothing,
      text : "test time",
      links: []
    }
  ]
}

expectedKeyChange :: Story
expectedKeyChange = {
  title : "test",
  screens: [
    {
      key : "changed",
      img : Nothing,
      text : "test time",
      links: []
    }
  ]
}

expectedTextChange :: Story
expectedTextChange = {
  title : "test",
  screens: [
    {
      key : "test",
      img : Nothing,
      text : "bing bong",
      links: []
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
      Assert.equal true $ isJust $ img =<< findScreen "start" =<< story
    test "Finds title of first screen" do
      story <- getTestStory
      Assert.equal (Just "Good morning Sean.") (text <$> (findScreen "start" =<< story))
    test "Counts 3 links in first story" do
      story <- getTestStory
      Assert.equal (Just 3) (length <$> links <$> (findScreen "start" =<< story))
  
    suite "Validating story" do
      test "Validates link is not valid" do
        story <- getTestStory
        let firstLink = head =<< links <$> (findScreen "start" =<< story)
        let valid = validateLink <$> story <*> firstLink
        Assert.equal (Just false) valid
    
    suite "Editing" do
      test "Change a key" do
        Assert.equal expectedKeyChange $ updateKey "test" "changed" testStory
      test "Change the text" do
        Assert.equal expectedTextChange $ updateText "test" "bing bong" testStory
        

    