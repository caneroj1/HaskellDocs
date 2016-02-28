{-# LANGUAGE OverloadedStrings #-}

module Views.ViewUtils where

import           Data.Monoid
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5            as HTML (div)
import           Text.Blaze.Html5.Attributes

bootstrapCss = "bootstrap/css/bootstrap.min.css"
bootstrapJs  = "bootstrap/js/bootstrap.min.js"
jqueryJs     = "jquery/jquery-2.1.4.min.js"
myJs         = "js/index.js"
myCss        = "css/styles.css"

styles = do
  link ! rel "stylesheet" ! type_ "text/css" ! href bootstrapCss
  link ! rel "stylesheet" ! type_ "text/css" ! href myCss

renderSideNav = do
  HTML.div ! class_ "col-md-3" $ do
    HTML.div ! class_ "side-nav-fixed" $ do
      ul ! class_ "nav nav-pills nav-stacked" $ do
        li $ do
          a ! href "/" ! class_ "sidebar-item" $ "Search"
        li $ do
          a ! href "/docs" ! class_ "sidebar-item" $ "All Docs"
        li $ do
          a ! href "#" ! class_ "sidebar-item" $ "Upload"

renderHead pageTitle = do
  Text.Blaze.Html5.head $ do
    Text.Blaze.Html5.title pageTitle
    meta ! charset "utf-8"
    meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    styles

renderJavascript = do
  script "" ! src jqueryJs
  script "" ! src bootstrapJs
  script "" ! src myJs

container htmlContent =
  HTML.div ! class_ "container" $
    HTML.div ! class_ "row-fluid" $ htmlContent

concatHtml :: [Html] -> Html
concatHtml = foldl (<>) mempty
