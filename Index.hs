{-# LANGUAGE DoAndIfThenElse            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Sequence              (Seq, foldlWithIndex, singleton,
                                             (<|), (|>))
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

updateDB :: Connection -> (Bool, OCR) -> Document -> IO ()
updateDB _      (False, _  ) doc = return ()
updateDB dbConn (_,     ocr) doc = do
  let tsvector = foldlWithIndex (\prev _ text -> append prev text) T.empty $
                 title doc <| " " <| ocr
  _ <- updateSearchable dbConn (Models.Document.docID doc) tsvector
  return ()

-- updateComponent :: Bool -> Document -> Connection -> IO ()
-- updateComponent _      (False, _  ) doc = return ()
-- updateComponent dbConn (_,     ocr) doc = do
--   let tsvector = foldlWithIndex (\prev _ text -> append prev text) T.empty $
--                  title doc <| " " <| ocr
--   _ <- updateSearchable dbConn (Models.Document.docID doc) tsvector
--   return ()

-- performOCR :: Connection -> Document -> IO (Bool, OCR)
-- performOCR dbConn document = do
--   -- comps <- getComponentsByDocID dbConn (Models.Document.docID document)
--   -- let totalToOCR = totalImages2 document comps
--   -- s <- filterTrue =<< mapM ocrForComponent comps
--   -- results <- filterTrue =<< ocrForDocument document comps s
--   -- putStr $ "Indexed " ++ show (length results) ++ "/" ++ show totalToOCR
--   -- putStr " images successfully\n\n"
--   -- let content  = foldl (|>) Seq.empty $ map snd results
--   -- let success = length results == totalToOCR
--   -- updateDB dbConn (success, content) document
--   -- return (success, content)
--   return (True, Seq.empty)

type Log = Seq String

printLog :: Log -> IO ()
printLog = mapM_ print

addLog :: String -> IndexRoutine ()
addLog = tell . singleton

data IndexData = IndexData {
    connection :: Connection
  , document   :: Document
  }

type IndexedCount = Int

data IndexResults = IndexResults {
    indexedCount :: IndexedCount
  , content      :: T.Text
  } deriving (Show)

inc :: IndexResults -> IndexResults
inc IndexResults { indexedCount = n, content = c} =
  IndexResults { indexedCount = n+1, content = c }

app :: T.Text -> IndexResults -> IndexResults
app t IndexResults { indexedCount = n, content = c} =
  IndexResults { indexedCount = n, content = append c spaced }
  where spaced = append " " t

newtype IndexRoutine a = IndexRoutine {
    runIndexer :: ReaderT IndexData ( WriterT Log ( StateT IndexResults IO )) a
  }
  deriving (Monad, MonadReader IndexData, MonadIO, Applicative, Functor,
            MonadState IndexResults, MonadWriter Log)

components :: IndexData -> IO [DocumentComponent]
components IndexData { connection = dbConn, document = doc } =
  getComponentsByDocID dbConn (Models.Document.docID doc)

execOCR :: String -> IndexRoutine Bool
execOCR filename = do
  currentTime <- liftIO currentUTC
  addLog $ "Processing " ++ filename ++ " - " ++ show currentTime
  -- convert our image into the correct image format for OCR
  liftIO $ convertForOCR filename outputFile

  -- run tesseract OCR on the image
  liftIO $ runTesseract outputFile

  -- some logging. obtain the results of OCR
  nextTime    <- liftIO currentUTC
  content     <- liftIO $ liftM lines $ hGetContents =<< tesseractOutput
  let content' = strip . T.unlines $ map (strip . pack) content
  let nullText = T.null content'
  if nullText
  then addLog "ERROR: OCR generated empty results"
  else addLog . unpack $ append "Contents: " content'
  addLog $ "Finished processing " ++ filename ++ ". "
  addLog $ "Took: " ++ timeDiff currentTime nextTime
  addLog "Cleaning up..."

  -- clean up after performing OCR
  liftIO tesseractCleanUp
  liftIO $ imageCleanUp outputFile

  -- when we OCR successfully, update the count of indexed components
  unless nullText (modify' inc)
  unless nullText (modify' $ app content')

  return (not nullText)
  where outputFile      = renamePNG filename
        tesseractOutput = openFile "output.txt" ReadMode

ocr :: IndexRoutine Bool
ocr = do
  comps <- ask >>= liftIO . components
  execOCR (unpack . Models.DocumentComponent.filename $ head comps)

  -- total <- ask >>= liftIO . totalImages comps

  -- s <- filterTrue =<< mapM ocrForComponent comps
  -- results <- filterTrue =<< ocrForDocument document comps s
  -- putStr $ "Indexed " ++ show (length results) ++ "/" ++ show totalToOCR
  -- putStr " images successfully\n\n"
  -- let content  = foldl (|>) Seq.empty $ map snd results
  -- let success = length results == totalToOCR
  -- updateDB dbConn (success, content) document
  -- return (success, content)

  return True

process :: Connection -> Document -> IO ()
process dbConn doc = do
  ((success, lg), results) <-
    runStateT (
      runWriterT (
        runReaderT (
          runIndexer ocr
          )
          idxData
        )
    ) idxRes

  putStrLn $ "Result: " ++ passed success
  putStrLn "\nLog:\n---"
  printLog lg

  putStrLn $ "Components Indexed: " ++ show (indexedCount results)
  putStrLn "\nContents:\n--------"
  print $ content results

  where idxData = IndexData{connection = dbConn, document = doc}
        idxRes  = IndexResults{indexedCount = 0, content = T.empty}
        passed False = "Failure"
        passed True  = "Success"

main :: IO ()
main = do
  putStrLn "Starting Indexing Process for HaskellDocs\n----------"

  putStrLn "Querying DB to get un-indexed documents."
  dbConn      <- connectToDB
  currentTime <- currentUTC
  docs        <- getSomeNonIndexed dbConn
  nextTime    <- currentUTC

  putStrLn $ "Finished querying DB. Took: " ++ timeDiff currentTime nextTime

  forM_ docs $ process dbConn
