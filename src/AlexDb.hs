{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  AlexDb.hs
Description :  File containing all Database access functions
Maintainer  :  <AlexGagne>

This file gives access to the MongoDB containing most of the text and data associated with the website.
-}

module AlexDb where

import Data.Text                    (pack, unpack, splitOn, Text)
import qualified System.Environment as E
import qualified Database.MongoDB   as M

runMongo functionToRun = do
    dbValue <- getDBValue
    let dbInfo = getDatabaseInformation dbValue
    let dbHost = unpack $ host dbInfo
    let dbUser = user dbInfo
    let dbPassword = password dbInfo
    let dbName = databaseName dbInfo
    pipe <- M.connect $ M.readHostPort dbHost
    success <- M.access pipe M.master dbName $ M.auth dbUser dbPassword
    if success
        then do 
            e <- M.access pipe M.master dbName functionToRun
            M.close pipe
    else M.close pipe

getDatabaseInformation :: String -> DatabaseInfo
getDatabaseInformation dbValue = DatabaseInfo dbUser dbPassword dbHost dbDatabaseName where
    dbUser = parseUser dbValue
    dbPassword = parsePassword dbValue
    dbHost = parseHost dbValue
    dbDatabaseName = parseDatabaseName dbValue

parseUser :: String -> Text
parseUser dbValue = splitOn (pack "//")
                            (splitOn (pack ":") (pack dbValue) !! 1) !! 1

parsePassword :: String -> Text
parsePassword dbValue = head $ splitOn (pack "@")
                                       (splitOn (pack ":") (pack dbValue) !! 2)

parseHost :: String -> Text
parseHost dbValue = head (splitOn (pack "/")
                                  (splitOn (pack "@") (pack dbValue) !! 1))

parseDatabaseName :: String -> Text
parseDatabaseName dbValue = last $ splitOn (pack "/") (pack dbValue)

-- | Gives the uri for mongodb in the following format: 
-- | mongodb://dbuser:dbpass@host:port/dbname
getDBValue :: IO String
getDBValue = E.getEnv "MONGODB_URI"

data DatabaseInfo = DatabaseInfo
    { user         :: Text
    , password     :: Text
    , host         :: Text
    , databaseName :: Text
    }
