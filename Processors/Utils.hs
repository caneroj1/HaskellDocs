module Processors.Utils where

import           Data.Monoid
import qualified Data.Text.Lazy     as T
import           Network.HTTP.Types
import           Web.Scotty         (ActionM, status)

type Errors = [T.Text]

exists :: Foldable t => t a -> Bool
exists  = not . null
textExists = not . T.null

verify :: Bool -> Bool -> Errors -> T.Text -> ActionM (Bool, Errors)
verify prev next prevErr msg
    | prev =
      if next then
        return (True, prevErr)
      else do
        status status400
        return (False, prevErr <> [msg])
    | otherwise = return (False, prevErr)
