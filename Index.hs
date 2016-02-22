{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Monad
import           Data.Text.Lazy             (append, pack, strip, unpack)
import qualified Data.Text.Lazy             as T (null, unlines)
import           Database.PostgreSQL.Simple
import           DB.Database
import           Models.Document
import           Models.DocumentComponent
import           System.IO
import           Utils.DateUtils
import           Utils.Helpers
import           Utils.ImageUtils
import           Utils.TesseractUtils

execOCR :: String -> IO Bool
execOCR filename = do
  let convertedFilename = renamePNG filename
  putStrLn $ "\n\nProcessing " ++ filename
  currentTime <- currentUTC
  convertForOCR filename convertedFilename
  runTesseract convertedFilename
  nextTime    <- currentUTC
  content <- liftM lines $ hGetContents =<< openFile "output.txt" ReadMode
  let content' = strip . T.unlines $ map (strip . pack) content
  if T.null content'
  then putStrLn "ERROR: OCR generated empty results"
  else putStrLn . unpack $ append "Contents: " content'
  putStr $ "Finished processing " ++ filename ++ ". "
  putStrLn $ "Took: " ++ timeDiff currentTime nextTime
  putStrLn "Cleaning up...\n\n"
  tesseractCleanUp
  imageCleanUp convertedFilename
  return . not $ T.null content'

ocrForDocumentComponents :: Connection -> Document -> IO Bool
ocrForDocumentComponents dbConn document = do
  comps <- getComponentsByDocID dbConn (Models.Document.docID document)
  s <- return <$> filter id =<< mapM (execOCR . tGlobalPath . filename) comps
  putStr $ "Indexed " ++ show (length s) ++ "/" ++ show (length comps)
  putStr " document components successfully\n\n"
  return (length s == length comps)

main :: IO ()
main = do
  putStrLn "Starting Indexing Process for HaskellDocs\n----------"

  putStrLn "Querying DB to get un-indexed documents."
  dbConn      <- connectToDB
  currentTime <- currentUTC
  docs        <- getSomeNonIndexed dbConn
  nextTime    <- currentUTC
  putStrLn $ "Finished querying DB. Took: " ++ timeDiff currentTime nextTime

  s <- return <$> filter id =<< mapM (ocrForDocumentComponents dbConn) docs

  putStr $ "Indexed " ++ show (length s) ++ "/" ++ show (length docs)
  putStr " document(s) successfully\n"
