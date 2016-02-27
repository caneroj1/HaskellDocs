{-# LANGUAGE OverloadedStrings #-}

import           Web.Scotty

import           Control.Monad
import qualified Data.ByteString.Lazy                 as BS
import qualified Data.Text.Lazy                       as T
import           Database.PostgreSQL.Simple           (Connection)
import           DB.Database
import           Models.Document
import           Models.DocumentComponent
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Network.Wai.Parse                    (FileInfo, fileContent,
                                                       fileName)
import           Processors.UploadProcessor
import           Text.Blaze.Html.Renderer.Text
import           Utils.DateUtils
import           Utils.Helpers
import           Views.AllDocuments
import           Views.Index

main :: IO ()
main = scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "assets")
    middleware logStdoutDev
    get "/" . html $ renderHtml Views.Index.index

    post "/upload" $ do
      dbConn <- liftAndCatchIO connectToDB
      processUploads dbConn

    -- get all documents
    get "/docs" $ do
      ds <- liftAndCatchIO $ getAllDocuments =<< connectToDB
      ts <- liftAndCatchIO $ mapM (utcToLocal . Models.Document.createdAt) ds
      html . renderHtml $ Views.AllDocuments.allDocuments $ zip ds ts

    get "/docs.json" $ do
      docs <- liftAndCatchIO $ getAllDocuments =<< connectToDB
      json docs

    -- get the document by id and its associated components
    get "/docs/:id" $ do
      dbConn     <- liftAndCatchIO connectToDB
      documentID <-  textToInteger <$> param "id"
      document   <- liftAndCatchIO $ getDocumentByID dbConn documentID
      components <- liftAndCatchIO $ getComponentsByDocID dbConn documentID
      json (document, components)

    get "/search" $ do
      dbConn <- liftAndCatchIO connectToDB
      q      <- liftM formatQuery $ param "query"
      docs   <- liftAndCatchIO $ getDocumentsByQuery dbConn q
      json docs
