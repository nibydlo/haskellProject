{-# LANGUAGE OverloadedStrings #-}

module Utils
  (
    cutTitle
  , getTextView
  , getTextViewText
  , getMaxId
  , lazyLenUtf8
  , numBytesUtf8
  , textViewGetValue
  , toLazy
  ) where

import Data.Text (pack, unpack, Text(..))
import qualified Data.Text.Lazy as Lazy (pack, toStrict, Text(..))
import Domain (id, Note(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import GI.Gtk ( castTo
              , textBufferGetEndIter
              , textBufferGetStartIter
              , textBufferGetText
              , textViewGetBuffer
              , textViewNew
              , TextView(..)
              , Widget(..))
import GHC.Int (Int32(..))

getMaxId :: [Note] -> Integer
getMaxId [] = (-1)
getMaxId notes = maximum . map Domain.id $ notes

cutTitle :: Text -> Text
cutTitle t  = pack $ take 16 $ unpack t

toLazy :: Text -> Lazy.Text
toLazy = Lazy.pack . unpack

numBytesUtf8 :: String -> Int
numBytesUtf8 = BS.length . UTF8.fromString

textViewGetValue :: TextView -> IO Text
textViewGetValue tv = do
  buf <- textViewGetBuffer tv
  start <- textBufferGetStartIter buf
  end <- textBufferGetEndIter buf
  value <- textBufferGetText buf start end True
  return value

getTextViewText :: Widget -> IO Text
getTextViewText w = do
  maybeTv <- castTo TextView w
  case maybeTv of
    Nothing -> return ""
    Just tv -> textViewGetValue tv

getTextView :: Widget -> IO TextView
getTextView w = do
  maybeTv <- castTo TextView w
  case maybeTv of
    Nothing -> do
      newTV <- textViewNew
      return newTV
    Just tv -> return tv

lazyLenUtf8 :: Lazy.Text -> Int32
lazyLenUtf8 t = ((fromIntegral ((numBytesUtf8 $ unpack $ Lazy.toStrict t) :: Int) :: Int32))
