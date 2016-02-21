{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.DocumentComponent
(
  DocumentComponent(..),
  createDocument,
  createDocuments,
  getAllDocuments,
  getDocumentByID,
  getSomeNonIndexed,
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
    , lastIndexedAt :: UTCTime
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
  }) = [toField dID, toField fileName]
