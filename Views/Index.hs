{-# LANGUAGE OverloadedStrings #-}

module Views.Index where

import           Data.Time.LocalTime
import           Models.Document
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5            as HTML (div, form, label)
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as Attr (id)
import qualified Views.Documents             as V
import           Views.ViewUtils

indexHeader = do
  h1 ! class_ "text-center" $ "Search your Documents"
  HTML.form ! method "get" ! action "/" $ do
    HTML.div ! class_ "form-group" $ do
      HTML.label ! for "query" $ "Search"
      input ! type_ "text"
            ! class_ "form-control"
            ! Attr.id "query"
            ! name "query"
    button ! type_ "submit" ! class_ "btn btn-default" $ "Find Documents"

displayResults :: [(Document, LocalTime)] -> Html
displayResults [] =
  HTML.div ! class_ "alert alert-info" $ do
    strong "Hey there! "
    "No results were found. Try a new search."

displayResults xs =
  concatHtml $ Prelude.map V.documentBlock xs

index :: [(Document, LocalTime)] -> Html
index docs =
  html $ do
    renderHead "My Documents"
    body $ do
      container $ do
          HTML.div ! class_ "col-md-8 col-md-offset-1" $ do
            indexHeader
            hr
            displayResults docs
          renderSideNav
      renderJavascript
