{-# LANGUAGE OverloadedStrings #-}

import           Web.Scotty

import qualified Data.ByteString.Lazy                 as BS
import qualified Data.Text.Lazy                       as T
import           Database.PostgreSQL.Simple           (Connection)
import           DB.Database
import           Models.Document
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Network.Wai.Parse                    (FileInfo, fileContent,
                                                       fileName)
import           Processors.UploadProcessor
import           Text.Blaze.Html.Renderer.Text
import           Utils.Helpers
import           Views.Index

main :: IO ()
main = scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "assets")
    middleware logStdoutDev
    get "/" . html $ renderHtml Views.Index.index

    -- upload a single document: single associated image to OCR and index
    post "/upload" $ do
      dbConn <- liftAndCatchIO connectToDB
      processUploads dbConn

    -- upload a single document: multiple associated images to OCR and index
    -- post "/upload/components"

    -- get all documents and their components
    get "/docs" $ do
      docs <- liftAndCatchIO $ getAllDocuments =<< connectToDB
      json docs

    -- get the document by id and its associated components
    get "/docs/:id" $ do
      dbConn <- liftAndCatchIO connectToDB
      documentID <-  textToInteger <$> param "id"
      json =<< liftAndCatchIO (getDocumentByID dbConn documentID)
