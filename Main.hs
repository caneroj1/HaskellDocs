{-# LANGUAGE OverloadedStrings #-}

import           Web.Scotty

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
import           Utils.Helpers
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
      docs <- liftAndCatchIO $ getAllDocuments =<< connectToDB
      json docs

    -- get the document by id and its associated components
    get "/docs/:id" $ do
      dbConn     <- liftAndCatchIO connectToDB
      documentID <-  textToInteger <$> param "id"
      document   <- liftAndCatchIO $ getDocumentByID dbConn documentID
      components <- liftAndCatchIO $ getComponentsByDocID dbConn documentID
      json (document, components)
