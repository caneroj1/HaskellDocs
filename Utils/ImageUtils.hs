module Utils.ImageUtils
(
  convertForOCR,
  rename,
  imageCleanUp
)
where

import           System.Directory
import           System.FilePath  (dropExtension)
import           System.Process

rename :: String -> String
rename fname = dropExtension fname ++ ".tif"

imagemagickCmd fname output =
  "convert " ++ fname ++ " -resize 400% -type Grayscale " ++ output

convertForOCR :: String -> String -> IO ()
convertForOCR filename output = callCommand $ imagemagickCmd filename output

imageCleanUp :: FilePath -> IO ()
imageCleanUp = removeFile
