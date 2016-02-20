{-# LANGUAGE OverloadedStrings #-}

module DB.Database
(
  connectToDB,
  close,
)
where

import           Control.Monad
import           Data.Word
import           Database.PostgreSQL.Simple
import           System.Environment

dbName :: IO String
dbName = getEnv "DBNAME"

dbHost :: IO String
dbHost = getEnv "DBHOST"

dbPass :: IO String
dbPass = getEnv "DBPASS"

dbUser :: IO String
dbUser = getEnv "DBUSER"

dbPort :: IO Word16
dbPort = do
  port <- getEnv "DBPORT"
  return $ fromIntegral (read port :: Int)

connection :: IO ConnectInfo
connection = do
  host <- dbHost
  port <- dbPort
  user <- dbUser
  pass <- dbPass
  name <- dbName
  return ConnectInfo {
    connectHost     = host,
    connectPort     = port,
    connectUser     = user,
    connectPassword = pass,
    connectDatabase = name
  }

connectToDB :: IO Connection
connectToDB = connection >>= connect
