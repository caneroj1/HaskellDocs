{-# LANGUAGE OverloadedStrings #-}
import           Web.Scotty

import           Control.Monad
import qualified Data.ByteString.Char8                as C (unpack)
import qualified Data.ByteString.Lazy                 as BS
import           Data.Monoid                          (mconcat)
import qualified Data.Text.Lazy                       as T
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Network.Wai.Parse                    (fileContent, fileName)
import           System.FilePath                      ((</>))
import           Text.Blaze.Html.Renderer.Text
import           Utils.DateUtils
import           Views.Index

jsonData :: (T.Text, Int, T.Text)
jsonData = ("Joe", 23, "Software Engineer")

main :: IO ()
main = scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "assets")
    middleware logStdoutDev
    get "/" $ do
      html $ mconcat ["<h1>Scotty, beam me up!</h1>"]
    get "/json" $ do
      json Main.jsonData
    get "/json/:name" $ do
      name <- param "name"
      json (name :: T.Text)
    get "/text/:name" $ do
      name <- param "name"
      text name
    get "/html" $ do
      html $ renderHtml Views.Index.index
    post "/upload" $ do
      processUploads

-- process each of the uploaded files.
-- we want to process the data for the files
-- in the following order:
-- 1. convert to grayscale and magnify
-- 2. pass to tesseract for OCR
-- 3. save path
processUploads :: ActionM ()
processUploads = do
  fs' <- files
  fAndNPairs <- liftAndCatchIO $ zipFilesWithTimes fs'
  liftAndCatchIO . sequence_ $ [ BS.writeFile (toPath path) (fileContent file)
    | ( (_, file), path) <- fAndNPairs ]
  json $ map snd fAndNPairs
  where toPath xs = "assets" </> xs

zipFilesWithTimes :: [File] -> IO [(File, String)]
zipFilesWithTimes fs = do
  let indexed = zipWith (\i (_, f) -> show i ++ toString f) [0..length fs] fs
  filenames' <- mapM toPath indexed
  return $ zip fs filenames'
  where timeString = liftM2 getTimeString currentTimeZone currentUTC
        toString f = C.unpack $ fileName f
        toPath xs  = (\x -> return $ x ++ "_" ++ xs) =<< timeString
