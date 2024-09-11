module Database.Sqlite.Adapter
  ( JSONField (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import Data.Data (Typeable)
import Database.SQLite.Simple
  ( ResultError (ConversionFailed),
    SQLData (SQLText),
  )
import Database.SQLite.Simple.FromField (FromField (fromField), returnError)
import Database.SQLite.Simple.ToField (ToField (toField))

-- | Newtype wrapper to serialize/deserialize field with json.
newtype JSONField a = JSONField a

instance (FromJSON a, Typeable a) => FromField (JSONField a) where
  fromField field = do
    text <- fromField field
    case eitherDecodeStrict' $ encodeUtf8 text of
      Left err -> returnError ConversionFailed field err
      Right val -> pure $ JSONField val

instance (ToJSON a) => ToField (JSONField a) where
  toField (JSONField val) = SQLText $ toStrict $ decodeUtf8 $ encode val
