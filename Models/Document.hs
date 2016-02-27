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
  updateSearchable,
  getDocumentsByQuery,
  toNewDoc
)
where

import           Control.Applicative                ((<$>))
import           Data.Aeson
import           Data.Int
import           Data.Monoid
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
    , mainFilename  :: Text.Text
    -- , searchable :: ??? tsvector in postgres
  } |
  NewDoc {
    title :: Text.Text
  , fname :: Text.Text
  }
  deriving (Show, Generic)

instance ToJSON Document
instance FromJSON Document

instance FromRow Document where
  fromRow = Doc <$>
  --        title     docID     createdAt indexed   indexedF  lastIndexedAt
            field <*> field <*> field <*> field <*> field <*> field <*>
  --        mainFilename
            field

instance ToRow Document where
  toRow (
    NewDoc {
    title = docTitle
  , fname = fileName
  }) = [toField docTitle, toField fileName]

toNewDoc :: Text.Text -> Text.Text -> Document
toNewDoc title filename =
  NewDoc { title = title
         , fname = filename }

multiSQL :: Query
multiSQL = "insert into Documents (title, mainFilename) values (?, ?)"
createDocuments :: Connection -> [Document] -> IO Int64
createDocuments connection = executeMany connection multiSQL

createSQL :: Query
createSQL = "insert into Documents (title, mainFilename) values (?, ?) \
            \returning title, docID, createdAt, indexed,               \
            \indexFailure, lastIndexedAt, mainFilename"
createDocument :: Connection -> Document -> IO Document
createDocument connection doc = head <$> query connection createSQL doc

indexSQL :: Query
indexSQL = "select title, docID, createdAt, indexed, indexFailure,     \
            \lastIndexedAt, mainFilename from Documents"
getAllDocuments :: Connection -> IO [Document]
getAllDocuments connection = query_ connection indexSQL

selectSQL :: Query
selectSQL = "select title, docID, createdAt, indexed, indexFailure,    \
            \lastIndexedAt, mainFilename, from Documents where docID = ?"
getDocumentByID :: Connection -> Integer -> IO Document
getDocumentByID connection documentID = do
  [document] <- query connection selectSQL [documentID]
  return document

nonIndexedSQL :: Query
nonIndexedSQL = "select title, docID, createdAt, indexed, indexFailure, \
                \lastIndexedAt, mainFilename from Documents             \
                \where indexed = false order by createdAt asc limit 15"
getSomeNonIndexed :: Connection -> IO [Document]
getSomeNonIndexed connection = query_ connection nonIndexedSQL

tsvectorSQL :: Query
tsvectorSQL = "update Documents set textSearchableColumn = \
               \to_tsvector('english', ?) where docid = ?"
updateSearchable :: Connection -> Integer -> Text.Text -> IO Int64
updateSearchable connection documentID text =
  execute connection tsvectorSQL (text, documentID)

querySQL :: Query
querySQL = "select title, docID, createdAt, indexed, indexFailure, \
           \lastIndexedAt, mainFilename from Documents             \
           \where textSearchableColumn @@ to_tsquery(?, ?)"
getDocumentsByQuery :: Connection -> Text.Text -> IO [Document]
getDocumentsByQuery dbConn queryStr = do
  putStrLn $ Text.unpack queryStr
  query dbConn querySQL ("english" :: Text.Text, queryStr)
