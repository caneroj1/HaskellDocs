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
import           Models.DocumentComponent
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
        timeText   = (T.append "_" . T.pack) <$> timeString
        assetsAndTime time name = tGlobalPath $ T.append time name
        toPath xs  = (\x -> return $ assetsAndTime x xs) =<< timeText

writeFiles :: [(UploadedFile, T.Text)] -> IO ()
writeFiles fs = forM_ fs (\(file, name) ->
  BS.writeFile (T.unpack name) (fileContent file))

doUpload :: Connection -> File -> [File] -> T.Text -> ActionM ()
doUpload dbConn mainImage fs title = do
  fAndNPairs    <- liftAndCatchIO $ zipFilesWithNames (mainImage : fs)
  liftAndCatchIO $ writeFiles fAndNPairs
  let mainFilename = snd $ head fAndNPairs
  doc <- liftAndCatchIO $ createDocument dbConn $ toNewDoc title mainFilename
  let filenames = map snd (tail fAndNPairs)
  let comps = zip filenames $ replicate (length fs) (Models.Document.docID doc)
  let newComps = map toNewComponent comps
  liftAndCatchIO $ createDocumentComponents dbConn newComps
  json (doc, newComps)

-- we need to have a title and at least one image uploaded
contentOK :: T.Text -> [File] -> ActionM (Bool, Errors)
contentOK title fs = do
  (b1, m1) <- verify True (textExists title) [] "Title not supplied."
  verify b1 (exists fs) m1 "No images were uploaded."

-- if we have multiple images, we must have one image specified via the
-- "main" keyword. that image won't be indexed but it will be
-- stored as the main image for a document, which may be composed
-- of smaller, more easy to OCR components
imagesOK :: [File] -> (Bool, Errors) -> ActionM (Bool, Errors)
imagesOK fs (b, errs)
  | not b = return (b, errs)
  | length fs == 1 = return (True, [])
  | otherwise =
    verify True (lc $ filter (("main" ==) . fst) fs) [] "No main image uploaded"
  where lc x = 1 == length x

splitFiles :: [File] -> (File, [File])
splitFiles fs
  | length fs == 1 = (head fs, [])
  | otherwise      = process [] fs
  where process prev ((name, d) : fss) = if name == "main"
                                         then ((name, d), prev ++ fss)
                                         else process (prev ++ [(name, d)]) fss

processUploads :: Connection -> ActionM ()
processUploads dbConn = do
  title    <- T.pack <$> param "title"
  fs       <- files
  (okay, errs) <- imagesOK fs =<< contentOK title fs
  let (main, others) = splitFiles fs
  if not okay then json (errs :: Errors)
  else doUpload dbConn main others title
