{-# LANGUAGE OverloadedStrings #-}

module Models.Document
(
  Document(..),
  createDocument,
  createDocuments,
  toNewDoc
)
where

import           Control.Applicative                ((<$>))
import           Data.Int
import qualified Data.Text.Lazy                     as Text
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           DB.Database

data Document =
  Doc {
      filename  :: Text.Text
    , title     :: Text.Text
    , docID     :: Int
    , createdAt :: UTCTime
  } |
  NewDoc {
      filename :: Text.Text
    , title    :: Text.Text
  }

instance FromRow Document where
  fromRow = Doc <$> field <*> field <*> field <*> field

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
