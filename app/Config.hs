module Config (RegexActionConfig (..), EventActionConfig (..), StaleFileConfig (..), AppConfig (..), parseConfigFromFile, directories) where

import Data.Time (NominalDiffTime)
import Data.Vector qualified as V
import Fmt (fmt, (+|), (|+))
import Predicate (EventType (..))
import Relude.Extra.Map (lookup)
import Text.Megaparsec (errorBundlePretty)
import Text.Regex.TDFA (Regex, makeRegexM)
import Text.Toml (Node (..), Table, parseTomlDoc)

showType :: Node -> Text
showType (VTable _) = "table"
showType (VTArray _) = "table array"
showType (VString _) = "string"
showType (VInteger _) = "integer"
showType (VFloat _) = "float"
showType (VBoolean _) = "boolean"
showType (VDatetime _) = "date"
showType (VArray _) = "array"

lookupValue :: Text -> Table -> Either Text Node
lookupValue key table =
    case lookup key table of
        Nothing -> Left $ "Missing key: " <> toText key
        Just (VTable _) -> Left $ "Expected value but got table for key: " <> toText key
        Just (VTArray _) -> Left $ "Expected value but got table array for key: " <> toText key
        Just val -> Right val

lookupArray :: Text -> Table -> Either Text [Node]
lookupArray key table =
    case lookup key table of
        Nothing -> Left $ "Missing key: " <> toText key
        Just (VArray arr) -> Right . V.toList $ arr
        Just val -> Left $ "Expected array but got " <> showType val <> " for key: " <> toText key

valueToText :: Node -> Either Text Text
valueToText (VString t) = Right t
valueToText val = Left $ "Expected string but got " <> showType val

valueToDouble :: Node -> Either Text Double
valueToDouble (VFloat d) = Right d
valueToDouble (VInteger i) = Right $ fromIntegral i
valueToDouble val = Left $ "Expected number but got " <> showType val

valueToNatural :: Node -> Either Text Natural
valueToNatural (VInteger i)
    | i >= 0 = Right $ fromIntegral i
    | otherwise = Left "Expected non-negative integer"
valueToNatural val = Left $ "Expected integer but got " <> showType val

parseRegex :: Text -> Either Text Regex
parseRegex t = case makeRegexM t of
    Just r -> Right r
    Nothing -> Left $ "Invalid regex: " <> toText t

parseTime :: Node -> Either Text NominalDiffTime
parseTime val = do
    d <- valueToDouble val
    return $ realToFrac d

parseEventType :: Text -> Either Text EventType
parseEventType "create" = Right Create
parseEventType "modify" = Right Modify
parseEventType "attribute" = Right ModifyAttribute
parseEventType "remove" = Right Remove
parseEventType "close" = Right Close
parseEventType "unknown" = Right UnknownEvent
parseEventType "delete_directory" = Right DeleteDirectory
parseEventType unk = Left . fmt $ "Unknown event type " +| unk |+ " expected one of create, modify, attribute, remove, close, unknown, or delete_directory."

data RegexActionConfig = RegexActionConfig
    { directory :: !FilePath
    , lineRegex :: !Regex
    , filepathRegex :: !Regex
    , name :: !Text
    , cmd :: !String
    }
    deriving (Generic)

parseRegexActionConfig :: Table -> Either Text RegexActionConfig
parseRegexActionConfig table = do
    dir <- lookupValue "directory" table >>= valueToText
    lineRegexText <- lookupValue "line_regex" table >>= valueToText
    filepathRegexText <- lookupValue "filepath_regex" table >>= valueToText
    nameText <- lookupValue "name" table >>= valueToText
    cmdText <- lookupValue "cmd" table >>= valueToText

    lineReg <- parseRegex lineRegexText
    filepathReg <- parseRegex filepathRegexText

    return
        RegexActionConfig
            { directory = toString dir
            , lineRegex = lineReg
            , filepathRegex = filepathReg
            , name = nameText
            , cmd = toString cmdText
            }

data StaleFileConfig = StaleFileConfig
    { directory :: !FilePath
    , filepathRegex :: !Regex
    , timeout :: !NominalDiffTime
    , name :: !Text
    , cmd :: !String
    }
    deriving (Generic)

parseStaleFileConfig :: Table -> Either Text StaleFileConfig
parseStaleFileConfig table = do
    dir <- lookupValue "directory" table >>= valueToText
    filepathRegexText <- lookupValue "filepath_regex" table >>= valueToText
    timeoutVal <- lookupValue "timeout" table
    nameText <- lookupValue "name" table >>= valueToText
    cmdText <- lookupValue "cmd" table >>= valueToText

    filepathReg <- parseRegex filepathRegexText
    timeoutTime <- parseTime timeoutVal

    return
        StaleFileConfig
            { directory = toString dir
            , filepathRegex = filepathReg
            , timeout = timeoutTime
            , name = nameText
            , cmd = toString cmdText
            }

data EventActionConfig = EventActionConfig
    { directory :: !FilePath
    , filepathRegex :: !Regex
    , eventTypes :: ![EventType]
    , name :: !Text
    , cmd :: !String
    }
    deriving (Generic)

parseEventActionConfig :: Table -> Either Text EventActionConfig
parseEventActionConfig table = do
    dir <- lookupValue "directory" table >>= valueToText
    filepathRegexText <- lookupValue "filepath_regex" table >>= valueToText
    eventTypesArray <- lookupArray "event_types" table
    nameText <- lookupValue "name" table >>= valueToText
    cmdText <- lookupValue "cmd" table >>= valueToText

    filepathReg <- parseRegex filepathRegexText
    eventTypeTexts <- mapM valueToText eventTypesArray
    eventTypesList <- mapM parseEventType eventTypeTexts

    return
        EventActionConfig
            { directory = toString dir
            , filepathRegex = filepathReg
            , eventTypes = eventTypesList
            , name = nameText
            , cmd = toString cmdText
            }

data AppConfig = AppConfig
    { regexActions :: [RegexActionConfig]
    , staleFileActions :: [StaleFileConfig]
    , eventActions :: [EventActionConfig]
    , queueLength :: Natural
    }

-- Helper function to parse arrays of tables
parseTableArray :: (Foldable f) => (Table -> Either Text a) -> f Table -> Either Text [a]
parseTableArray parser = mapM parser . toList

parseAppConfig :: Table -> Either Text AppConfig
parseAppConfig table = do
    regexActionsArray <- case lookup "regex_actions" table of
        Nothing -> Right []
        Just (VTArray arr) -> parseTableArray parseRegexActionConfig arr
        Just val -> Left $ "Expected array for regex_actions but got " <> showType val

    staleFileActionsArray <- case lookup "stale_file_actions" table of
        Nothing -> Right []
        Just (VTArray arr) -> parseTableArray parseStaleFileConfig arr
        Just val -> Left $ "Expected array for stale_file_actions but got " <> showType val

    eventActionsArray <- case lookup "event_actions" table of
        Nothing -> Right []
        Just (VTArray arr) -> parseTableArray parseEventActionConfig arr
        Just val -> Left $ "Expected array for event_actions but got " <> showType val

    queueLengthVal <- lookupValue "queue_length" table
    queueLen <- valueToNatural queueLengthVal

    return
        AppConfig
            { regexActions = regexActionsArray
            , staleFileActions = staleFileActionsArray
            , eventActions = eventActionsArray
            , queueLength = queueLen
            }

directories :: AppConfig -> [FilePath]
directories config =
    ordNub $
        map (.directory) config.regexActions
            <> map (.directory) config.staleFileActions
            <> map (.directory) config.eventActions

parseConfig :: FilePath -> ByteString -> Either Text AppConfig
parseConfig filepath content =
    first show (decodeUtf8' content)
        >>= first (toText . errorBundlePretty) . (parseTomlDoc filepath)
        >>= parseAppConfig

parseConfigFromFile :: FilePath -> IO (Either Text AppConfig)
parseConfigFromFile filepath = do
    content <- readFileBS filepath
    pure (parseConfig filepath content)
