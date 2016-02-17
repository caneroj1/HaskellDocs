{-# LANGUAGE OverloadedStrings #-}

module Views.Index where

import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes
import           Views.ViewUtils

index = do
  html $ do
    renderHead "Index"
    body $ do
      h1 "Hey!"
      h2 "What's up?"
      renderJavascript
