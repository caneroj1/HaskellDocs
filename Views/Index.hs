{-# LANGUAGE OverloadedStrings #-}

module Views.Index where

import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes
import           Views.ViewUtils

index = do
  html $ do
    renderHead "Index"
    body $ do
      h1 "Placeholder Text"
      h2 "Nothing here yet.."
      renderJavascript
