{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Monad
import           Data.Sequence              (Seq, foldlWithIndex, (<|), (|>))
import qualified Data.Sequence              as Seq (empty)
import           Data.Text.Lazy             (append, pack, strip, unpack)
import qualified Data.Text.Lazy             as T (Text, empty, null, unlines)
import           Database.PostgreSQL.Simple
import           DB.Database
import           Models.Document
import           Models.DocumentComponent
import           System.IO
import           Utils.DateUtils
import           Utils.Helpers
import           Utils.ImageUtils
import           Utils.TesseractUtils

type OCR = Seq T.Text

execOCR :: String -> IO (Bool, T.Text)
execOCR filename = do
  let convertedFilename = renamePNG filename
  putStrLn $ "\n\nProcessing " ++ filename
  currentTime <- currentUTC
  convertForOCR filename convertedFilename
  runTesseract convertedFilename
  nextTime    <- currentUTC
  content     <- liftM lines $ hGetContents =<< openFile "output.txt" ReadMode
  let content' = strip . T.unlines $ map (strip . pack) content
  if T.null content'
  then putStrLn "ERROR: OCR generated empty results"
  else putStrLn . unpack $ append "Contents: " content'
  putStr $ "Finished processing " ++ filename ++ ". "
  putStrLn $ "Took: " ++ timeDiff currentTime nextTime
  putStrLn "Cleaning up...\n\n"
  tesseractCleanUp
  imageCleanUp convertedFilename
  return (not $ T.null content', append content' " ")

ocrForDocument :: Document -> [DocumentComponent] ->
                  [(Bool, T.Text)] -> IO [(Bool, T.Text)]
ocrForDocument doc [] _ =
  liftM ( : [] ) $ execOCR . unpack $ mainFilename doc
ocrForDocument _ components results = return results

ocrForComponent :: DocumentComponent -> IO (Bool, T.Text)
ocrForComponent component = execOCR (unpack $ filename component)

totalImages :: Document -> [DocumentComponent] -> Int
totalImages doc []       = 1
totalImages _   l@(x:xs) = length l

filterTrue :: [(Bool, a)] -> IO [(Bool, a)]
filterTrue = return <$> filter fst

updateDB :: Connection -> (Bool, OCR) -> Document -> IO ()
updateDB _      (False, _  ) doc = return ()
updateDB dbConn (_,     ocr) doc = do
  let tsvector = foldlWithIndex (\prev _ text -> append prev text) T.empty $
                 title doc <| " " <| ocr
  _ <- updateSearchable dbConn (Models.Document.docID doc) tsvector
  return ()

performOCR :: Connection -> Document -> IO (Bool, OCR)
performOCR dbConn document = do
  comps <- getComponentsByDocID dbConn (Models.Document.docID document)
  let totalToOCR = totalImages document comps
  s <- filterTrue =<< mapM ocrForComponent comps
  results <- filterTrue =<< ocrForDocument document comps s
  putStr $ "Indexed " ++ show (length results) ++ "/" ++ show totalToOCR
  putStr " images successfully\n\n"
  let content  = foldl (|>) Seq.empty $ map snd results
  let success = length results == totalToOCR
  updateDB dbConn (success, content) document
  return (success, content)

main :: IO ()
main = do
  putStrLn "Starting Indexing Process for HaskellDocs\n----------"

  putStrLn "Querying DB to get un-indexed documents."
  dbConn      <- connectToDB
  currentTime <- currentUTC
  docs        <- getSomeNonIndexed dbConn
  nextTime    <- currentUTC
  putStrLn $ "Finished querying DB. Took: " ++ timeDiff currentTime nextTime

  s <- filterTrue =<< mapM (performOCR dbConn) docs

  putStr $ "Indexed " ++ show (length s) ++ "/" ++ show (length docs)
  putStr " document(s) successfully\n"
