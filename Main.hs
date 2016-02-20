{-# LANGUAGE OverloadedStrings #-}

import           Web.Scotty

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8                as C (unpack)
import qualified Data.ByteString.Lazy                 as BS
import           Data.Monoid                          (mconcat)
import qualified Data.Text.Lazy                       as T
import           Database.PostgreSQL.Simple           (Connection)
import           DB.Database
import           Models.Document
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Network.Wai.Parse                    (FileInfo, fileContent,
                                                       fileName)
import           System.FilePath                      ((</>))
import           Text.Blaze.Html.Renderer.Text
import           Utils.DateUtils
import           Utils.Helpers
import           Utils.ImageUtils
import           Utils.TesseractUtils
import           Views.Index

type UploadedFile = FileInfo BS.ByteString

main :: IO ()
main = scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "assets")
    middleware logStdoutDev
    get "/" $ do
      html $ renderHtml Views.Index.index
    post "/upload" $ do
      dbConn <- liftAndCatchIO connectToDB
      processUploads dbConn

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
  let fnames = map T.pack . pairsToStrings . indexed $ map (fileName . snd) fs
  filenames' <- mapM toPath fnames
  let files  = map snd fs
  return $ zip files filenames'
  where timeString = liftM2 getTimeString currentTimeZone currentUTC
        timeText = (T.append "_" . T.pack) <$> timeString
        toPath xs  = (\x -> return $ T.append x xs) =<< timeText

writeFiles :: [(UploadedFile, T.Text)] -> IO ()
writeFiles fs = forM_ fs (\(file, name) ->
  BS.writeFile (toPath name) (fileContent file))
  where toPath s = "assets" </> T.unpack s

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
  -- liftAndCatchIO $ mapM (execOCR . toPath . snd) fAndNPairs
  json $ map snd fAndNPairs
  where toPath xs = "assets" </> T.unpack xs

execOCR :: String -> IO ()
execOCR filename = do
  convertForOCR filename
  runTesseract $ rename filename
