{-# LANGUAGE OverloadedStrings #-}

module Processors.UploadProcessor where

import           Control.Monad
import qualified Data.ByteString.Lazy       as BS
import qualified Data.Text.Lazy             as T
import           Database.PostgreSQL.Simple (Connection)
import           DB.Database
import           Models.Document
import           Network.Wai.Parse          (FileInfo, fileContent, fileName)
import           Utils.DateUtils
import           Utils.Helpers
import           Web.Scotty                 (ActionM, File, Param, files, json,
                                             liftAndCatchIO, params)

type UploadedFile = FileInfo BS.ByteString

collectTitles :: Int -> [Param] -> [T.Text]
collectTitles n ps
  | n > psLen  = titles ++ replicate diff "Untitled"
  | n == psLen = titles
  | n < psLen  = take n titles
  where psLen  = length ps
        titles = map snd ps
        diff   = n - psLen

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

processUploads :: Connection -> ActionM ()
processUploads dbConn = do
  fs'        <- files
  ps         <- params
  fAndNPairs <- liftAndCatchIO $ zipFilesWithNames fs'
  liftAndCatchIO $ writeFiles fAndNPairs
  let titles    = collectTitles (length fs') ps
  let filenames = map snd fAndNPairs
  let docs      = zipWith (curry toNewDoc) filenames titles
  liftAndCatchIO $ createDocuments dbConn docs
  json $ map snd fAndNPairs
