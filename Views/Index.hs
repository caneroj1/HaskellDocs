{-# LANGUAGE OverloadedStrings #-}

module Views.Index where

import           Text.Blaze.Html5
import qualified Text.Blaze.Html5            as HTML (div, form, label)
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as Attr (id)
import           Views.ViewUtils

indexHeader = do
  h1 ! class_ "text-center" $ "Search your Documents"
  HTML.form ! method "get" ! action "/search" $ do
    HTML.div ! class_ "form-group" $ do
      HTML.label ! for "query" $ "Search"
      input ! type_ "text"
            ! class_ "form-control"
            ! Attr.id "query"
            ! name "query"
    button ! type_ "submit" ! class_ "btn btn-default" $ "Find Documents"

index =
  html $ do
    renderHead "My Documents"
    body $ do
      container $ do
          HTML.div ! class_ "col-md-8 col-md-offset-1" $ do
            indexHeader
            hr
          renderSideNav
      renderJavascript
