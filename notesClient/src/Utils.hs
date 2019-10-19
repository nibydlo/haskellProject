module Utils
  (
    cutTitle
  , getMaxId
  , numBytesUtf8
  , toLazy
  ) where

import Data.Text (pack, unpack, Text(..))
import qualified Data.Text.Lazy as Lazy (pack, Text(..))
import Domain (id, Note(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8

getMaxId :: [Note] -> Integer
getMaxId = maximum . map Domain.id

cutTitle :: Text -> Text
cutTitle t  = pack $ take 16 $ unpack t

toLazy :: Text -> Lazy.Text
toLazy = Lazy.pack . unpack

numBytesUtf8 :: String -> Int
numBytesUtf8 = BS.length . UTF8.fromString
