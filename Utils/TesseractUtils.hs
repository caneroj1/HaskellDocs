module Utils.TesseractUtils
(
  runTesseract
)
where

import           System.Process

tesseractCmd fname = "tesseract -l eng " ++ fname ++ " output"

runTesseract :: String -> IO ()
runTesseract filename = callCommand $ tesseractCmd filename
