{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.DocumentComponent
(
  DocumentComponent(..),
  createDocumentComponents,
  getComponentsByDocID,
  toNewComponent
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

data DocumentComponent =
  Component {
      componentID   :: Integer
    , docID         :: Integer
    , filename      :: Text.Text
    , createdAt     :: UTCTime
    , indexed       :: Bool
    , indexFailure  :: Bool
    , lastIndexedAt :: Maybe UTCTime
    -- , searchable :: ??? tsvector in postgres
  } |
  NewComponent {
      filename :: Text.Text
    , docID    :: Integer
  }
  deriving (Show, Generic)

instance ToJSON DocumentComponent
instance FromJSON DocumentComponent

instance FromRow DocumentComponent where
  fromRow = Component <$>
  --        compID    docID     filename  created
            field <*> field <*> field <*> field <*>
  --        indexed   indexF    lastIndexed
            field <*> field <*> field

instance ToRow DocumentComponent where
  toRow (
    NewComponent {
      filename = fileName
    , docID = dID
  }) = [toField fileName, toField dID]

toNewComponent :: (Text.Text, Integer) -> DocumentComponent
toNewComponent (filename, docID) =
  NewComponent { filename = filename, docID = docID }

multiSQL :: Query
multiSQL = "insert into DocumentComponents (filename, docID) values (?, ?)"
createDocumentComponents :: Connection -> [DocumentComponent] -> IO Int64
createDocumentComponents connection = executeMany connection multiSQL

selectSQL :: Query
selectSQL = "select componentID, docID, filename, createdAt, indexed, \
            \indexFailure, lastIndexedAt from DocumentComponents where \
            \docID = ?"
getComponentsByDocID :: Connection -> Integer -> IO [DocumentComponent]
getComponentsByDocID dbConn docID =
  query dbConn selectSQL [docID]
