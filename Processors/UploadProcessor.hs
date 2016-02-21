{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE OverloadedStrings #-}

module Processors.UploadProcessor where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy       as BS
import           Data.Monoid
import qualified Data.Text.Lazy             as T
import           Database.PostgreSQL.Simple (Connection)
import           DB.Database
import           Models.Document
import           Models.Document
import           Network.HTTP.Types
import           Network.Wai.Parse          (FileInfo, fileContent, fileName)
import           Processors.Utils
import           Utils.DateUtils
import           Utils.Helpers
import           Web.Scotty                 (ActionM, File, Param, files, json,
                                             liftAndCatchIO, param, params,
                                             raise, status)

type UploadedFile = FileInfo BS.ByteString

zipFilesWithNames :: [File] -> IO [(UploadedFile, T.Text)]
zipFilesWithNames fs = do
  let fnames = map T.pack . pairsToStrings . indexes $ map (fileName . snd) fs
  filenames' <- mapM toPath fnames
  let files  = map snd fs
  return $ zip files filenames'
  where timeString = liftM2 getTimeString currentTimeZone currentUTC
        timeText = (T.append "_" . T.pack) <$> timeString
        toPath xs  = (\x -> return $ T.append x xs) =<< timeText

writeFiles :: [(UploadedFile, T.Text)] -> IO ()
writeFiles fs = forM_ fs (\(file, name) ->
  BS.writeFile (tGlobalPath name) (fileContent file))

doUpload :: Connection -> [File] -> T.Text -> ActionM ()
doUpload dbConn fs title = do
  fAndNPairs <- liftAndCatchIO $ zipFilesWithNames fs
  liftAndCatchIO $ writeFiles fAndNPairs
  let filenames = map snd fAndNPairs
  let document  = toNewDoc title
  let docs      = zipWith (curry toNewDoc) filenames titles
  liftAndCatchIO $ createDocuments dbConn docs
  json $ map snd fAndNPairs

processUploads :: Connection -> ActionM ()
processUploads dbConn = do
  title    <- T.pack <$> param "title"
  fs       <- files
  (b1, m1) <- verify True (textExists title) [] "Title not supplied."
  (b2, m2) <- verify b1 (exists fs) m1 "No images were uploaded."
  if not b2 then json (m2 :: Errors)
  else doUpload dbConn fs title
