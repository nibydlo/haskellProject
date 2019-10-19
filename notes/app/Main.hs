{-# LANGUAGE OverloadedStrings #-}

module Main where

import Dao ( listNotes
          , findNote
          , insertNote
          , updateNote
          , deleteNote
          , newConn
          , DbConfig(..))
import Views ( notesList
             , viewNote
             , createdNote
             , updatedNote
             , deletedNote)
import Domain (Note(..))
import Web.Scotty ( body
                  , delete
                  , get
                  , middleware
                  , param
                  , post
                  , put
                  , scotty
                  , ActionM(..))
import Web.Scotty.Internal.Types (ActionT(..))
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Configurator as C (load, lookup)
import Data.Configurator.Types (Config(..), Worth(..))
import Database.MySQL.Simple (close)
import Data.Pool(Pool, createPool, withResource)
import Data.Text.Lazy (Text(..))
import Data.Aeson (decode)

makeDbConfig :: Config -> IO (Maybe Dao.DbConfig)
makeDbConfig conf = do
  name <- C.lookup conf "database.name" :: IO (Maybe String)
  user <- C.lookup conf "database.user" :: IO (Maybe String)
  password <- C.lookup conf "database.password" :: IO (Maybe String)
  return $ DbConfig <$> name
                    <*> user
                    <*> password


getNoteParam :: ActionT Text IO (Maybe Note)
getNoteParam = do
  b <- body
  return $ (decode b :: Maybe Note)
    where makeNote s = ""


main :: IO ()
main = do
    loadedConf <- C.load [Required "application.conf"]
    dbConf <- makeDbConfig loadedConf

    case dbConf of
      Nothing   -> putStrLn "No database configuration found"
      Just conf -> do
          pool <- createPool (newConn conf) close 1 64 10
          scotty 3000 $ do
              middleware $ logStdout

              get "/notes" $ do
                notes <- liftIO $ listNotes pool
                notesList notes

              get "/notes/:id" $ do
                id <- param "id" :: ActionM Text
                maybeNote <- liftIO $ findNote pool id
                viewNote maybeNote

              post "/notes" $ do
                note <- getNoteParam
                insertNote pool note
                liftIO $ putStrLn (show note)
                createdNote note

              put "/notes" $ do
                note <- getNoteParam
                updateNote pool note
                updatedNote note

              delete "/notes/:id" $ do
                id <- param "id" :: ActionM Text
                deleteNote pool id
                deletedNote id
