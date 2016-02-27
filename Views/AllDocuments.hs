{-# LANGUAGE OverloadedStrings #-}

module Views.AllDocuments where

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

documentBlock :: (Doc.Document, LocalTime) -> Html
documentBlock (doc, localTime) = do
  h2 $ (toHtml . Doc.title) doc
  p $ do
    strong "Uploaded: "
    toHtml $ timeToString localTime
  img ! class_ "document-image"
      ! src (toValue . toSrc $ Doc.mainFilename doc)

allDocuments :: [(Doc.Document, LocalTime)] -> Html
allDocuments docsAndTimes =
  html $ do
    renderHead "All Documents"
    body $ do
      container $
          HTML.div ! class_ "col-md-10 col-md-offset-1" $ do
            h1 ! class_ "text-center" $ "All Documents"
            concatHtml $ Prelude.map documentBlock docsAndTimes
      renderJavascript
