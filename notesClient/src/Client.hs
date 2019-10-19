{-# LANGUAGE OverloadedStrings #-}

module Client
  (
    deleteNoteRequest
  , getNote
  , getNotes
  , postNote
  , putNote
  ) where

import Control.Lens.Getter ((^.))
import Control.Lens.Operators ((^?))
import Domain (Note(..))
import Data.Aeson (decode, toJSON, decode)
import Data.Aeson.Lens (key)
import Data.Maybe (isNothing)
import Network.Wreq (delete, get, post, put, responseBody, responseHeader)

getNoteFromMaybe :: (Maybe Note) -> Note
getNoteFromMaybe Nothing = Note (-1) "no such note" ""
getNoteFromMaybe (Just n) = n

getNote :: Integer -> IO Note
getNote noteId = do
  r <- get $ "http://localhost:3000/notes/" ++ (show noteId)
  --let decoded = decode (r ^. responseBody) :: Maybe Note
  let isNote = r ^? responseBody . key "id"
  let decoded = if (isNothing isNote)
                then Nothing
                else decode (r ^. responseBody) :: Maybe Note
  return $ getNoteFromMaybe decoded

getNotesFromMaybe :: (Maybe [Note]) -> [Note]
getNotesFromMaybe Nothing = []
getNotesFromMaybe (Just notes) = notes

getNotes :: IO ([Note])
getNotes = do
  r <- get "http://localhost:3000/notes"
  let decoded = decode (r ^. responseBody) :: Maybe [Note]
  return $ getNotesFromMaybe decoded

postNote :: Note -> IO ()
postNote note = do
  post "http://localhost:3000/notes" (toJSON  note)
  return ()

putNote :: Note -> IO ()
putNote note = do
  put "http://localhost:3000/notes" (toJSON  note)
  return ()

deleteNoteRequest :: Integer -> IO ()
deleteNoteRequest noteId = do
  r <- delete $ "http://localhost:3000/notes/" ++ (show noteId)
  return ()
