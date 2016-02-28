{-# LANGUAGE OverloadedStrings #-}

module Views.AllDocuments where

import           Data.Maybe
import qualified Data.Text.Lazy              as T (unpack)
import           Data.Time.LocalTime
import qualified Models.Document             as Doc
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5            as HTML (div, form, label)
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as Attr (id)
import           Utils.DateUtils
import           Utils.Helpers
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

statusBlock :: Doc.Document -> Html
statusBlock (Doc.Doc {  Doc.lastIndexedAt = l,
                        Doc.indexed       = i})
  | not i = do
    HTML.div ! class_ "alert alert-danger w40" $ do
      strong "Indexing Failed "
      "Please retry indexing"
    button ! class_ "display-block btn btn-info m20b" $ "Re-index"
  | i     =
    HTML.div ! class_ "alert alert-success w40" $ do
      strong "Indexing Succeeded "
      "This document is now searchable"
  | isNothing l =
    HTML.div ! class_ "alert alert-warning w40" $ do
      strong "Not yet indexed "
      "This document has not been indexed"

documentBlock :: (Doc.Document, LocalTime) -> Html
documentBlock (doc, localTime) = do
  h2 $ (toHtml . Doc.title) doc
  p $ do
    strong "Uploaded: "
    toHtml $ timeToString localTime
  statusBlock doc
  img ! class_ "w50 img-thumbnail"
      ! src (toValue . toSrc $ Doc.mainFilename doc)

allDocuments :: [(Doc.Document, LocalTime)] -> Html
allDocuments docsAndTimes =
  html $ do
    renderHead "All Documents"
    body $ do
      container $ do
          HTML.div ! class_ "col-md-8 col-md-offset-1" $ do
            h1 ! class_ "text-center" $ "All Documents"
            concatHtml $ Prelude.map documentBlock docsAndTimes
          renderSideNav
      renderJavascript
