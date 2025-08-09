module Predicate (filepathMatch, eventMatch, EventType (..), newOrModifyFile, anyEventMatch, getEventType) where

import System.FSNotify
import Text.Regex.TDFA (Regex, match)

filepathMatch :: Regex -> ActionPredicate
filepathMatch filepathRegex = match filepathRegex . eventPath

data EventType = Create | Modify | DeleteDirectory | UnknownEvent | ModifyAttribute | Remove | Close deriving (Show, Eq)

-- Extract the EventType from an ActionPredicate
getEventType :: Event -> EventType
getEventType (Added _ _ _) = Create
getEventType (Modified _ _ _) = Modify
getEventType (ModifiedAttributes _ _ _) = ModifyAttribute
getEventType (Removed _ _ _) = Remove
getEventType (CloseWrite _ _ _) = Close
getEventType (WatchedDirectoryRemoved _ _ _) = DeleteDirectory
getEventType (Unknown _ _ _ _) = UnknownEvent

-- Rewritten eventMatch as composition
eventMatch :: EventType -> ActionPredicate
eventMatch expectedType = (== expectedType) . getEventType

anyEventMatch :: [EventType] -> ActionPredicate
anyEventMatch types ev = any (\evType -> eventMatch evType ev) types

newOrModifyFile :: ActionPredicate
newOrModifyFile (Added _ _ IsFile) = True
newOrModifyFile (Modified _ _ IsFile) = True
newOrModifyFile _ = False
