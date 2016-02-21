import           Control.Monad
import           Data.Text.Lazy       (unpack)
import           DB.Database
import           Models.Document
import           System.IO
import           Utils.DateUtils
import           Utils.ImageUtils
import           Utils.TesseractUtils

execOCR :: String -> IO ()
execOCR filename = do
  putStrLn $ "Processing " ++ filename
  currentTime <- currentUTC
  convertForOCR filename
  runTesseract $ rename filename
  nextTime    <- currentUTC
  putStrLn "Contents:"
  content <- liftM lines $ hGetContents =<< openFile "output" ReadMode
  forM_ content putStrLn
  print $ "Finished processing " ++ filename ++ ". "
  putStrLn $ "Took: " ++ timeDiff currentTime nextTime

main :: IO ()
main = do
  putStrLn "Starting Indexing Process for HaskellDocs\n----------"

  putStrLn "Querying DB to get un-indexed documents."
  currentTime <- currentUTC
  documents   <- getSomeNonIndexed =<< connectToDB
  nextTime    <- currentUTC
  putStrLn $ "Finished querying DB. Took: " ++ timeDiff currentTime nextTime
  forM_ (map (unpack . filename) documents) execOCR
  print $ "Indexed " ++ show (length documents) ++ " document(s) successfully"
