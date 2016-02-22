module Utils.ImageUtils
(
  convertForOCR,
  renamePNG,
  renameTIF,
  imageCleanUp
)
where

import           Data.Monoid
import           System.Directory
import           System.FilePath  (dropExtension)
import           System.Process

rename :: String -> FilePath -> FilePath
rename ext fname = dropExtension fname ++ ext

renamePNG :: FilePath -> FilePath
renamePNG = rename ".png"

renameTIF :: FilePath -> FilePath
renameTIF = rename ".tif"

grayscale :: String
grayscale = "-type Grayscale"

resize :: Int -> String
resize n = "-resize " ++ show n ++ "%"

contrast :: String
contrast = "-contrast"

sharpen :: Double -> String
sharpen d = "-sharpen " ++ show d

convert = "convert"

(<.>) :: String -> String -> String
(<.>) l r = l <> " " <> r

convertForOCR :: FilePath -> FilePath -> IO ()
convertForOCR inFile outFile = callCommand $
  convert     <.> grayscale <.>
  resize 200  <.> contrast  <.>
  sharpen 1.0 <.> inFile    <.> outFile

imageCleanUp :: FilePath -> IO ()
imageCleanUp = removeFile
