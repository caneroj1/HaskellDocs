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
import           Network.Wai.Parse                    (fileContent, fileName)
import           System.FilePath                      ((</>))
import           Text.Blaze.Html.Renderer.Text
import           Utils.DateUtils
import           Utils.ImageUtils
import           Utils.TesseractUtils
import           Views.Index

main :: IO ()
main = scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "assets")
    middleware logStdoutDev
    get "/" $ do
      html $ renderHtml Views.Index.index
    post "/upload" $ do
      dbConn <- liftAndCatchIO connectToDB
      processUploads dbConn

-- process each of the uploaded files.
-- we want to process the data for the files
-- in the following order:
-- 1. convert to grayscale and magnify
-- 2. pass to tesseract for OCR
-- 3. save path
processUploads :: Connection -> ActionM ()
processUploads dbConn = do
  fs' <- files
  ps  <- params
  fAndNPairs <- liftAndCatchIO $ zipFilesWithTimes fs'
  liftAndCatchIO . sequence_ $ [ BS.writeFile (toPath  path) (fileContent file)
    | ( (_, file), path) <- fAndNPairs ]
  let titles    = collectTitles (length fs') ps
  let filenames = map snd fAndNPairs
  let docs      = zipWith (curry toNewDoc) filenames titles
  liftAndCatchIO $ createDocuments dbConn docs
  -- liftAndCatchIO $ mapM (execOCR . toPath . snd) fAndNPairs
  json $ map snd fAndNPairs
  where toPath xs = "assets" </> T.unpack xs

collectTitles :: Int -> [Param] -> [T.Text]
collectTitles n ps
  | n > psLen  = titles ++ replicate diff "Untitled"
  | n == psLen = titles
  | n < psLen  = take n titles
  where psLen  = length ps
        titles = map snd ps
        diff   = psLen - n

indexed :: [a] -> [(Int, a)]
indexed xs = zip [0..length xs] xs

pairsToStrings :: (Show a, Show b) => [(a, b)] -> [String]
pairsToStrings = map (\(x, y) -> show x ++ show y)

zipFilesWithTimes :: [File] -> IO [(File, T.Text)]
zipFilesWithTimes fs = do
  let filenames = map T.pack . pairsToStrings . indexed $ map (fileName.snd) fs
  filenames' <- mapM toPath filenames
  return $ zip fs filenames'
  where timeString = liftM2 getTimeString currentTimeZone currentUTC
        timeText = (T.append "_" . T.pack) <$> timeString
        toPath xs  = (\x -> return $ T.append x xs) =<< timeText

execOCR :: String -> IO ()
execOCR filename = do
  convertForOCR filename
  runTesseract $ rename filename
