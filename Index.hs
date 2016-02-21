import           Control.Monad
import           Data.Text.Lazy       (unpack)
import           DB.Database
import           Models.Document
import           System.IO
import           Utils.ImageUtils
import           Utils.TesseractUtils

execOCR :: String -> IO ()
execOCR filename = do
  putStrLn $ "Processing " ++ filename
  convertForOCR filename
  runTesseract $ rename filename
  content <- liftM lines $ hGetContents =<< openFile "output" ReadMode
  forM_ content putStrLn
  putStrLn $ "Finished processing " ++ filename

main :: IO ()
main = do
  putStrLn "Starting Indexing Process for HaskellDocs\n----------"

  documents  <- getSomeNonIndexed =<< connectToDB
  forM_ (map (unpack . filename) documents) execOCR

  print $ "Indexed " ++ show (length documents) ++ " document(s) successfully"
