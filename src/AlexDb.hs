{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  AlexDb.hs
Description :  File containing all Database access functions
Maintainer  :  <AlexGagne>

This file gives access to the MongoDB containing most of the text and data associated with the website.
-}

module AlexDb  (Post) where

import Data.Time.Clock              (UTCTime)
import Data.Time.Clock.POSIX        (POSIXTime, posixSecondsToUTCTime)
import Data.Text                    (pack, unpack, splitOn, Text)
import qualified System.Environment as E
import qualified Database.MongoDB   as DB
import Database.MongoDB             ((!?))

-- getAllBlogPosts :: Action IO[Post]

documentToPost :: DB.Document -> Post
documentToPost doc = Post postTitle postContent postPostedDate postLastModifiedDate where
    postTitle = pack $ getString (pack "title") doc
    postContent = pack $ getString (pack "content") doc 
    postPostedDate = posixSecondsToUTCTime ((fromIntegral $ getInteger "postedDate" doc) :: POSIXTime)
    postLastModifiedDate = posixSecondsToUTCTime ((fromIntegral $ getInteger "lastModifiedDate" doc) :: POSIXTime)

allPosts :: DB.Action IO [DB.Document]
allPosts = DB.rest =<< DB.find (DB.select [] "posts")

runMongo functionToRun = do
    dbValue <- getDBValue
    let dbInfo = getDatabaseInformation dbValue
    let dbHost = unpack $ host dbInfo
    let dbUser = user dbInfo
    let dbPassword = password dbInfo
    let dbName = databaseName dbInfo
    pipe <- DB.connect $ DB.readHostPort dbHost
    success <- DB.access pipe DB.master dbName $ DB.auth dbUser dbPassword
    if success
        then do 
            e <- DB.access pipe DB.master dbName functionToRun
            DB.close pipe
    else DB.close pipe

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


-- Data Types
data DatabaseInfo = DatabaseInfo
    { user         :: Text
    , password     :: Text
    , host         :: Text
    , databaseName :: Text
    }

data Post = Post
    { title :: Text
    , content :: Text
    , postedDate :: UTCTime
    , lastModifiedDate :: UTCTime
    }

-- Utilities
getString :: DB.Label -> DB.Document -> String
getString label = DB.typed . DB.valueAt label

getInteger :: DB.Label -> DB.Document -> Integer
getInteger label = DB.typed . DB.valueAt label

getObjId :: DB.Document -> DB.ObjectId
getObjId = DB.typed . DB.valueAt "_id" 

getSecondaryObjId :: DB.Label -> DB.Document -> DB.ObjectId
getSecondaryObjId label = DB.typed . DB.valueAt label 

lookupString :: DB.Label -> DB.Document -> Maybe String
lookupString label document =
  document !? label

lookupInteger :: DB.Label -> DB.Document -> Maybe Integer
lookupInteger label document =
  document !? label

lookupSecondaryObjId :: DB.Label -> DB.Document -> Maybe DB.ObjectId
lookupSecondaryObjId label document =
  document !? label