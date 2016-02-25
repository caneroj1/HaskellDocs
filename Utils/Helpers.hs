{-# LANGUAGE OverloadedStrings #-}

module Utils.Helpers where

import qualified Data.Text.Lazy      as Text
import           Data.Text.Lazy.Read
import           System.FilePath     ((</>))

indexes :: [a] -> [(Int, a)]
indexes xs = zip [0..length xs] xs

pairsToStrings :: (Show a, Show b) => [(a, b)] -> [String]
pairsToStrings = map (\(x, y) -> removeQuotes $ show x ++ show y)

removeQuotes :: String -> String
removeQuotes = filter (/= '"')

textToInteger :: Text.Text -> Integer
textToInteger text = go $ decimal text
  where go (Left  _)        = 0
        go (Right (num, _)) = num

tGlobalPath :: Text.Text -> Text.Text
tGlobalPath = Text.append "assets/"
