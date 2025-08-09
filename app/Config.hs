module Config (RegexActionConfig (..), EventActionConfig (..), StaleFileConfig (..), AppConfig (..), parseConfigFromFile, directories) where

import Predicate (EventType (..))

import Data.Time (NominalDiffTime)
import Fmt ((+|), (|+))
import Text.Regex.TDFA (Regex, makeRegexM)
import Toml
import Toml.Schema

instance FromValue Regex where
    fromValue (Text' _ t) = makeRegexM t
    fromValue _ = fail "Cannot make regex from non-text value."

instance FromValue NominalDiffTime where
    fromValue (Double' _ x) = pure $ realToFrac x
    fromValue (Integer' _ x) = pure . realToFrac . fromIntegral $ x
    fromValue _ = fail "Cannot make time from non-numeric value."

data RegexActionConfig = RegexActionConfig
    { directory :: !FilePath
    , lineRegex :: !Regex
    , filepathRegex :: !Regex
    , name :: !Text
    , cmd :: !String
    }
    deriving (Generic)
    deriving (FromValue) via GenericTomlTable RegexActionConfig

data StaleFileConfig
    = StaleFileConfig
    { directory :: !FilePath
    , filepathRegex :: !Regex
    , timeout :: !NominalDiffTime
    , name :: !Text
    , cmd :: !String
    }
    deriving (Generic)
    deriving (FromValue) via GenericTomlTable StaleFileConfig

instance FromValue EventType where
    fromValue (Text' _ "create") = pure Create
    fromValue (Text' _ "modify") = pure Modify
    fromValue (Text' _ "attribute") = pure ModifyAttribute
    fromValue (Text' _ "remove") = pure Remove
    fromValue (Text' _ "close") = pure Close
    fromValue (Text' _ "unknown") = pure UnknownEvent
    fromValue (Text' _ "delete_directory") = pure DeleteDirectory
    fromValue (Text' _ unk) = fail $ "Unknown event type " +| unk |+ " expected one of create, modify, attribute, remove or close."
    fromValue _ = fail "Cannot make event from non-text values"

data EventActionConfig = EventActionConfig
    { directory :: !FilePath
    , filepathRegex :: !Regex
    , eventTypes :: ![EventType]
    , name :: !Text
    , cmd :: !String
    }
    deriving (Generic)
    deriving (FromValue) via GenericTomlTable EventActionConfig

data AppConfig = AppConfig
    { regexActions :: [RegexActionConfig]
    , staleFileActions :: [StaleFileConfig]
    , eventActions :: [EventActionConfig]
    , queueLength :: Natural
    }

 instance FromValue AppConfig where



directories :: AppConfig -> [FilePath]
directories config =
    ordNub $
        map (.directory) config.regexActions
            <> map (.directory) config.staleFileActions
            <> map (.directory) config.eventActions

-- Convenience function to parse from file
parseConfigFromFile :: FilePath -> IO (Result String AppConfig)
parseConfigFromFile filepath = do
    bytestream <- readFileBS filepath
    case decodeUtf8Strict @Text @ByteString bytestream of
        Right content -> return $ decode content
        Left err -> return $ Failure [show err]
