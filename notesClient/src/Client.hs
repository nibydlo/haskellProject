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
import Domain (Note(..))
import Data.Aeson (decode, toJSON, decode)
import Network.Wreq (delete, get, post, put, responseBody)

getNoteFromMaybe :: (Maybe Note) -> Note
getNoteFromMaybe Nothing = Note (111111111111) "no such note" ""
getNoteFromMaybe (Just n) = n

getNote :: Integer -> IO Note
getNote noteId = do
  r <- get $ "http://localhost:3000/notes/" ++ (show noteId)
  let decoded = decode (r ^. responseBody) :: Maybe Note
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
