{-# LANGUAGE OverloadedStrings #-}

module Domain where

import Data.Text.Lazy (Text(..))
import Data.Aeson (
                    object
                  , (.=)
                  , (.=)
                  , (.:)
                  , (.:?)
                  , (.!=)
                  , FromJSON(..)
                  , Value(..)
                  , ToJSON(..))
import Control.Applicative ((<$>), (<*>))

data Note = Note { id :: Integer
                 , title :: Text
                 , text :: Text
                 }
     deriving (Show)

instance FromJSON Note where
     parseJSON (Object v) = Note <$>
                            v .:? "id" .!= 0 <*> 
                            v .:  "title"    <*>
                            v .:  "text"

instance ToJSON Note where
     toJSON (Note id title text) =
         object ["id" .= id,
                 "title" .= title,
                 "text" .= text]
