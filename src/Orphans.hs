{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans where

import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (encode, decodeLenient)
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Text as Text
import Data.Text (Text)
import Web.HttpApiData

encodeBase64 :: ByteString -> String
encodeBase64 = unpack . encode

decodeBase64 :: String -> ByteString
decodeBase64 = decodeLenient . pack

instance ToJSON ByteString where
  toJSON = toJSON . encodeBase64

instance FromJSON ByteString where
  parseJSON v = decodeBase64 `fmap` parseJSON v

instance FromHttpApiData ByteString where
  parseUrlPiece txt = Right $ decodeBase64 $ Text.unpack txt
