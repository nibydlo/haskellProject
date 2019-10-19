{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Dao
  (
    listNotes
  , findNote
  , insertNote
  , updateNote
  , deleteNote
  , newConn
  , DbConfig(..)
  ) where

import Domain (Note(..))
import Web.Scotty.Internal.Types (ActionT)
import GHC.Generics (Generic)
import Control.Monad.IO.Class (liftIO)
import Database.MySQL.Base (Connection(..))
import Database.MySQL.Simple ( connect
                             , connectDatabase
                             , connectPassword
                             , connectUser
                             , defaultConnectInfo
                             , execute
                             , query
                             , query_
                             , withTransaction
                             , Connection(..))
import Database.MySQL.Simple.Types ( Only(..)
                                   , Query(..))
import Database.MySQL.Simple.QueryResults (QueryResults(..))
import Database.MySQL.Simple.QueryParams (QueryParams(..))
import Data.Pool( Pool
                , createPool
                , withResource)
import qualified Data.Text.Lazy as TL (Text(..))
import qualified Data.Text.Lazy.Encoding as TL (decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as BL (pack)
import GHC.Int (Int64(..))

data DbConfig = DbConfig {
     dbName :: String,
     dbUser :: String,
     dbPassword :: String
     }
     deriving (Show, Generic)


newConn :: DbConfig -> IO Connection
newConn conf = connect defaultConnectInfo
                       { connectUser = dbUser conf
                       , connectPassword = dbPassword conf
                       , connectDatabase = dbName conf
                       }

fetch :: (QueryResults r, QueryParams q) => Pool Connection -> q -> Query -> IO [r]
fetch pool args sql = withResource pool retrieve
      where retrieve conn = query conn sql args

fetchSimple :: QueryResults r => Pool Connection -> Query -> IO [r]
fetchSimple pool sql = withResource pool retrieve
       where retrieve conn = query_ conn sql

execSqlT :: QueryParams q => Pool Connection -> q -> Query -> IO Int64
execSqlT pool args sql = withResource pool ins
       where ins conn = withTransaction conn $ execute conn sql args

listNotes :: Pool Connection -> IO [Note]
listNotes pool = do
  res <- fetchSimple pool "SELECT * FROM note ORDER BY id DESC" :: IO [(Integer, TL.Text, TL.Text)]
  return $ map (\(id, title, text) -> Note id title text) res

findNote :: Pool Connection -> TL.Text -> IO (Maybe Note)
findNote pool id = do
     res <- fetch pool (Only id) "SELECT * FROM note WHERE id=?" :: IO [(Integer, TL.Text, TL.Text)]
     return $ oneNote res
     where oneNote ((id, title, text) : _) = Just $ Note id title text
           oneNote _ = Nothing

insertNote :: Pool Connection -> Maybe Note -> ActionT TL.Text IO ()
insertNote pool Nothing = return ()
insertNote pool (Just (Note id title text)) = do
  liftIO $ execSqlT pool (id, title, text)
    "INSERT INTO note(id, title, text) VALUES(?,?,?)"
  return ()

updateNote :: Pool Connection -> Maybe Note -> ActionT TL.Text IO ()
updateNote pool Nothing = return ()
updateNote pool (Just (Note id title bodyText)) = do
     liftIO $ execSqlT pool [title, bodyText, (TL.decodeUtf8 $ BL.pack $ show id)]
                            "UPDATE note SET title=?, text=? WHERE id=?"
     return ()

deleteNote :: Pool Connection -> TL.Text -> ActionT TL.Text IO ()
deleteNote pool id = do
     liftIO $ execSqlT pool [id] "DELETE FROM note WHERE id=?"
     return ()
