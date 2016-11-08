{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  AlexDb.hs
Description :  File containing all Database access functions
Maintainer  :  <AlexGagne>

This file gives access to the MongoDB containing most of the text and data associated with the website.
-}

module AlexDb (Post, getAllBlogPosts, title, content, postedDate, lastModifiedDate) where

import Data.Time.Clock              (UTCTime)
import Data.Time.Clock.POSIX        (POSIXTime, posixSecondsToUTCTime)
import Data.Text                    (pack, unpack, splitOn, Text)
import qualified System.Environment as E
import qualified Database.MongoDB   as Db
import Database.MongoDB             ((!?))

getAllBlogPosts :: IO [Post]
getAllBlogPosts = fmap (map documentToPost) $ runMongo allPosts

documentToPost :: Db.Document -> Post
documentToPost doc = Post postTitle postContent postPostedDate postLastModifiedDate where
    postTitle = pack $ getString (pack "title") doc
    postContent = pack $ getString (pack "content") doc 
    postPostedDate = posixSecondsToUTCTime ((fromIntegral $ getInteger "postedDate" doc) :: POSIXTime)
    postLastModifiedDate = posixSecondsToUTCTime ((fromIntegral $ getInteger "lastModifiedDate" doc) :: POSIXTime)

allPosts :: Db.Action IO [Db.Document]
allPosts = Db.rest =<< Db.find (Db.select [] "posts")

runMongo :: Db.Action IO a -> IO a
runMongo functionToRun = do
    dbValue <- getDbValue
    let dbInfo = getDatabaseInformation dbValue
    let dbHost = unpack $ host dbInfo
    let dbUser = user dbInfo
    let dbPassword = password dbInfo
    let dbName = databaseName dbInfo
    pipe <- Db.connect $ Db.readHostPort dbHost
    Db.access pipe Db.master dbName $ Db.auth dbUser dbPassword
    e <- Db.access pipe Db.master dbName functionToRun
    Db.close pipe
    return e

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
getDbValue :: IO String
getDbValue = E.getEnv "MONGODB_URI"


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
getString :: Db.Label -> Db.Document -> String
getString label = Db.typed . Db.valueAt label

getInteger :: Db.Label -> Db.Document -> Integer
getInteger label = Db.typed . Db.valueAt label

getObjId :: Db.Document -> Db.ObjectId
getObjId = Db.typed . Db.valueAt "_id" 

getSecondaryObjId :: Db.Label -> Db.Document -> Db.ObjectId
getSecondaryObjId label = Db.typed . Db.valueAt label 

lookupString :: Db.Label -> Db.Document -> Maybe String
lookupString label document =
  document !? label

lookupInteger :: Db.Label -> Db.Document -> Maybe Integer
lookupInteger label document =
  document !? label

lookupSecondaryObjId :: Db.Label -> Db.Document -> Maybe Db.ObjectId
lookupSecondaryObjId label document =
  document !? label