{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Document
(
  Document(..),
  createDocument,
  createDocuments,
  getAllDocuments,
  getDocumentByID,
  getSomeNonIndexed,
  toNewDoc
)
where

import           Control.Applicative                ((<$>))
import           Data.Aeson
import           Data.Int
import qualified Data.Text.Lazy                     as Text
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           DB.Database
import           GHC.Generics

data Document =
  Doc {
      filename  :: Text.Text
    , title     :: Text.Text
    , docID     :: Integer
    , createdAt :: UTCTime
    , indexed   :: Bool
    -- , indexedAt :: UTCTime
    -- , searchable :: ??? tsvector in postgres
  } |
  NewDoc {
      filename :: Text.Text
    , title    :: Text.Text
  }
  deriving (Show, Generic)

instance ToJSON Document
instance FromJSON Document

instance FromRow Document where
  fromRow = Doc <$> field <*> field <*> field <*> field <*> field

instance ToRow Document where
  toRow (
    NewDoc {
      filename = docName
    , title = docTitle
  }) = [toField docName, toField docTitle]

toNewDoc :: (Text.Text, Text.Text) -> Document
toNewDoc (filename, title) =
  NewDoc { filename = filename, title = title }

createSQL :: Query
createSQL = "insert into Documents (filename, title) values (?, ?) \
            \returning (filename, title, docID, createdAt)"
createDocument :: Connection -> Document -> IO [Document]
createDocument connection = query connection createSQL

multiSQL :: Query
multiSQL = "insert into Documents (filename, title) values (?, ?)"
createDocuments :: Connection -> [Document] -> IO Int64
createDocuments connection = executeMany connection multiSQL

indexSQL :: Query
indexSQL = "select filename, title, docID, createdAt, indexed from Documents"
getAllDocuments :: Connection -> IO [Document]
getAllDocuments connection = query_ connection indexSQL

selectSQL :: Query
selectSQL = "select filename, title, docID, createdAt, indexed from Documents \
            \where docID = ?"
getDocumentByID :: Connection -> Integer -> IO Document
getDocumentByID connection documentID = do
  [document] <- query connection selectSQL [documentID]
  return document

nonIndexedSQL :: Query
nonIndexedSQL = "select filename, title, docID, createdAt, indexed from \
                \Documents where indexed = false order by createdAt asc \
                \limit 15"
getSomeNonIndexed :: Connection -> IO [Document]
getSomeNonIndexed connection = query_ connection nonIndexedSQL
