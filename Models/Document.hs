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
      title         :: Text.Text
    , docID         :: Integer
    , createdAt     :: UTCTime
    , indexed       :: Bool
    , indexFailure  :: Bool
    , lastIndexedAt :: Maybe UTCTime
    -- , searchable :: ??? tsvector in postgres
  } |
  NewDoc {
    title :: Text.Text
  }
  deriving (Show, Generic)

instance ToJSON Document
instance FromJSON Document

instance FromRow Document where
  fromRow = Doc <$>
  --        title     docID     createdAt indexed   indexedF  lastIndexedAt
            field <*> field <*> field <*> field <*> field <*> field

instance ToRow Document where
  toRow (
    NewDoc {
    title = docTitle
  }) = [toField docTitle]

toNewDoc :: Text.Text -> Document
toNewDoc (title) =
  NewDoc { title = title }

multiSQL :: Query
multiSQL = "insert into Documents (title) values (?)"
createDocuments :: Connection -> [Document] -> IO Int64
createDocuments connection = executeMany connection multiSQL

createSQL :: Query
createSQL = "insert into Documents (title) values (?) \
            \returning title, docID, createdAt, indexed, indexFailure, lastIndexedAt"
createDocument :: Connection -> Document -> IO Document
createDocument connection doc = head <$> query connection createSQL doc

indexSQL :: Query
indexSQL = "select title, docID, createdAt, indexed, indexFailure, lastIndexedAt \
            \from Documents"
getAllDocuments :: Connection -> IO [Document]
getAllDocuments connection = query_ connection indexSQL

selectSQL :: Query
selectSQL = "select title, docID, createdAt, indexed, indexFailure, lastIndexedAt \
            \from Documents where docID = ?"
getDocumentByID :: Connection -> Integer -> IO Document
getDocumentByID connection documentID = do
  [document] <- query connection selectSQL [documentID]
  return document

nonIndexedSQL :: Query
nonIndexedSQL = "select title, docID, createdAt, indexed, indexFailure, lastIndexedAt from \
                \Documents where indexed = false order by createdAt asc \
                \limit 15"
getSomeNonIndexed :: Connection -> IO [Document]
getSomeNonIndexed connection = query_ connection nonIndexedSQL
