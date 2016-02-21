import           Control.Monad
import           DB.Database
import           Models.Document
import           Utils.ImageUtils
import           Utils.TesseractUtils

execOCR :: String -> IO ()
execOCR filename = do
  convertForOCR filename
  runTesseract $ rename filename

main :: IO ()
main = do
  putStrLn "Starting Indexing Process for HaskellDocs\n----------"

  documents  <- getSomeNonIndexed =<< connectToDB

  print $ "Indexed " ++ show (length documents) ++ " document(s) successfully"
