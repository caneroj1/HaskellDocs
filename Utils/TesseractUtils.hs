module Utils.TesseractUtils
(
  runTesseract,
  tesseractCleanUp
)
where

import           System.Directory
import           System.Process

tesseractCmd fname = "tesseract -l eng " ++ fname ++ " output"

runTesseract :: String -> IO ()
runTesseract filename = callCommand $ tesseractCmd filename

tesseractCleanUp :: IO ()
tesseractCleanUp = removeFile "output.txt"
