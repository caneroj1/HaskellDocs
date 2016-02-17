{-# LANGUAGE OverloadedStrings #-}

module Views.ViewUtils where

import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes

bootstrapCss = "bootstrap/css/bootstrap.min.css"
bootstrapJs  = "bootstrap/js/bootstrap.min.js"
jqueryJs     = "jquery/jquery-2.1.4.min.js"

bootstrap = do
  link ! rel "stylesheet" ! type_ "text/css" ! href bootstrapCss

renderHead pageTitle = do
  Text.Blaze.Html5.head $ do
    Text.Blaze.Html5.title pageTitle
    meta ! charset "utf-8"
    meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    bootstrap

renderJavascript = do
  script "" ! src jqueryJs
  script "" ! src bootstrapJs
