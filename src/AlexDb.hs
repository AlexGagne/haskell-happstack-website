{-# LANGUAGE OverloadedStrings #-}

module AlexDB where

import qualified System.Environment as E
import qualified Data.Text as T


parseDbValue :: String -> DatabaseInfo
parseDbValue dbValue = DatabaseInfo dbUser dbPassword dbHost dbDatabaseName where
    dbUser = parseUser dbValue
    dbPassword = parsePassword dbValue
    dbHost = parseHost dbValue
    dbDatabaseName = parseDatabaseName dbValue

parseUser :: String -> String
parseUser dbValue = T.drop 2 $ (T.splitOn (T.pack ":") $ T.pack dbValue !! 1

parsePassword :: String -> String
parsePassword dbValue = "user"

parseHost :: String -> String
parseHost dbValue = T.unpack $ (T.splitOn (T.pack "/") $ (T.splitOn (T.pack "@") $ T.pack dbValue) !! 1) !! 0

parseDatabaseName :: String -> String
parseDatabaseName dbValue = "user"

getBDValue :: IO String
getBDValue = E.getEnv "MONGODB_URI"


data DatabaseInfo = DatabaseInfo{
    user         :: String,
    password     :: String,
    host         :: String,
    databaseName :: String } deriving (Show, Eq)

--mongodb://dbuser:dbpass@host:port/dbname
