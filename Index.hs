{-# LANGUAGE DoAndIfThenElse #-}

import           Control.Monad
import           Data.Text.Lazy       (pack, strip, unpack)
import qualified Data.Text.Lazy       as T (null, unlines)
import           DB.Database
import           Models.Document
import           System.IO
import           Utils.DateUtils
import           Utils.Helpers
import           Utils.ImageUtils
import           Utils.TesseractUtils

execOCR :: String -> IO ()
execOCR filename = do
  let convertedFilename = rename filename
  putStrLn $ "Processing " ++ filename
  currentTime <- currentUTC
  convertForOCR filename convertedFilename
  runTesseract convertedFilename
  nextTime    <- currentUTC
  putStrLn "Contents:"
  content <- liftM lines $ hGetContents =<< openFile "output.txt" ReadMode
  let content' = T.unlines $ map (strip . pack) content
  if T.null content'
  then putStrLn "Warning: OCR generated empty results"
  else print content'
  print $ "Finished processing " ++ filename ++ ". "
  putStrLn $ "Took: " ++ timeDiff currentTime nextTime
  putStrLn "Cleaning up..."
  tesseractCleanUp
  imageCleanUp convertedFilename

main :: IO ()
main = do
  putStrLn "Starting Indexing Process for HaskellDocs\n----------"

  putStrLn "Querying DB to get un-indexed documents."
  currentTime <- currentUTC
  documents   <- getSomeNonIndexed =<< connectToDB
  nextTime    <- currentUTC
  putStrLn $ "Finished querying DB. Took: " ++ timeDiff currentTime nextTime

  forM_ (map (tGlobalPath . filename) documents) execOCR

  print $ "Indexed " ++ show (length documents) ++ " document(s) successfully"
