{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views
  (
    notesList
  , viewNote
  , createdNote
  , updatedNote
  , deletedNote
  ) where

import Domain (Note(..))
import GHC.Generics (Generic)
import Web.Scotty (json, ActionM(..))
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as TL (Text(..))

notesList :: [Note] -> ActionM ()
notesList notes = json notes

viewNote :: Maybe Note -> ActionM ()
viewNote Nothing = json ()
viewNote (Just note) = json note

createdNote :: Maybe Note -> ActionM ()
createdNote Nothing = json ()
createdNote (Just note) = json note

updatedNote :: Maybe Note -> ActionM ()
updatedNote note = json ()

deletedNote :: TL.Text -> ActionM ()
deletedNote id = json ()
