module App.OnTextAreaInput where

import Prelude

import Data.Maybe (Maybe(..))
import Hedwig.Foreign (Trait, on)

import Web.DOM.Node as Node
import Web.Event.Event as Event
import Web.HTML.HTMLTextAreaElement as HTMLTextAreaElement

onTextAreaInput :: forall msg. (String -> msg) -> Trait msg
onTextAreaInput f = on "input" $ \event ->
    do
      let maybeTextAreaElement = event # Event.target >>= Node.fromEventTarget >>= HTMLTextAreaElement.fromNode
          
      value <- case maybeTextAreaElement of
        Just inputElement -> HTMLTextAreaElement.value inputElement
        Nothing -> pure ""
      pure $ f value
