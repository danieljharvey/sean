module Test.Main where

import Prelude

import Effect.Aff (Aff)
import Effect (Effect)
import App.Story (Story, findScreen, img, links, parseStory, screens, text, title, validateLink)
import Data.Maybe (Maybe(..), isJust)
import Data.Array (length, head)
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

import Node.FS.Aff as FS
import Node.Encoding (Encoding(..))

getTestStory :: Aff (Maybe Story)
getTestStory = do
  fileContents <- FS.readTextFile UTF8 "./test/test.json"
  pure $ parseStory fileContents

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
    

    