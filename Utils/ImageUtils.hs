module Utils.ImageUtils
(
  convertForOCR,
  rename
)
where

import           System.FilePath (dropExtension)
import           System.Process

rename :: String -> String
rename fname = dropExtension fname ++ ".tif"

imagemagickCmd fname output =
  "convert " ++ fname ++ " -resize 400% -type Grayscale " ++ output

convertForOCR :: String -> IO ()
convertForOCR filename = callCommand $ imagemagickCmd filename (rename filename)
