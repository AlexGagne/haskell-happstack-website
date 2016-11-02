{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  AlexDb.hs
Description :  File containing all Database access functions
Maintainer  :  <AlexGagne>

This file gives access to the MongoDB containing most of the text and data associated with the website.
-}

module AlexDb where

import qualified Data.Text          as T
import qualified System.Environment as E

getDatabaseInformation :: String -> DatabaseInfo
getDatabaseInformation dbValue = DatabaseInfo dbUser dbPassword dbHost dbDatabaseName where
    dbUser = parseUser dbValue
    dbPassword = parsePassword dbValue
    dbHost = parseHost dbValue
    dbDatabaseName = parseDatabaseName dbValue
    
parseUser :: String -> String
parseUser dbValue = T.unpack $ T.splitOn (T.pack "//")
                              (T.splitOn (T.pack ":") (T.pack dbValue) !! 1) !! 1

parsePassword :: String -> String
parsePassword dbValue = T.unpack $ head $ T.splitOn (T.pack "@")
                                         (T.splitOn (T.pack ":") (T.pack dbValue) !! 2)

parseHost :: String -> String
parseHost dbValue = T.unpack $ head (T.splitOn (T.pack "/")
                                    (T.splitOn (T.pack "@") (T.pack dbValue) !! 1))

parseDatabaseName :: String -> String
parseDatabaseName dbValue = T.unpack $ last $ T.splitOn (T.pack "/") (T.pack dbValue)

-- | Gives the uri for mongodb in the following format: mongodb://dbuser:dbpass@host:port/dbname
getDBValue :: IO String
getDBValue = E.getEnv "MONGODB_URI"

data DatabaseInfo = DatabaseInfo
    { user         :: String
    , password     :: String
    , host         :: String
    , databaseName :: String
    }
