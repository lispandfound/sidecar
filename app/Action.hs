module Action (regexWatcher, NotifyEvent (..), eventWatcher, staleFileUpdate, Action (..), staleFileChecker) where

import Conduit (
    ConduitT,
    MonadResource,
    filterC,
    linesUnboundedAsciiC,
    mapC,
    runConduitRes,
    sourceIOHandle,
    (.|),
 )
import Config (EventActionConfig (..), RegexActionConfig (..), StaleFileConfig (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TBQueue (TBQueue, writeTBQueue)
import Data.Conduit.TQueue (sinkTBQueue)
import Data.Map qualified as Map
import Data.Time (UTCTime)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Predicate (
    EventType (Create, Modify, ModifyAttribute),
    anyEventMatch,
    filepathMatch,
    newOrModifyFile,
 )
import Relude.Extra.Map (insert, keys, lookupDefault)
import System.Directory (getFileSize)
import System.FSNotify (
    Event (eventPath, eventTime),
 )
import System.FSNotify qualified as FS
import System.IO (SeekMode (..), hSeek, openBinaryFile)
import Text.Regex.TDFA (match)

data NotifyEvent
    = MatchEvent RegexActionConfig Event ByteString
    | FileEvent EventActionConfig Event
    | StaleEvent StaleFileConfig UTCTime FilePath

type OffsetMap = TVar (Map FilePath Integer)

data Action = Action
    { predicate :: FS.ActionPredicate
    , action :: FS.Action
    }

sourceFileFrom ::
    (MonadResource m) =>
    FilePath ->
    Integer ->
    ConduitT i ByteString m ()
sourceFileFrom fp pos = sourceIOHandle alloc
  where
    alloc = do
        handle <- openBinaryFile fp ReadMode
        hSeek handle AbsoluteSeek pos
        return handle

regexAction :: OffsetMap -> RegexActionConfig -> TBQueue NotifyEvent -> FS.Action
regexAction offsetMap config eventChannel event = do
    lastOffset <- atomically (lookupDefault 0 event.eventPath <$> readTVar offsetMap)
    runConduitRes
        ( sourceFileFrom event.eventPath lastOffset
            .| linesUnboundedAsciiC
            .| filterC (match config.lineRegex)
            .| mapC (MatchEvent config event)
            .| sinkTBQueue eventChannel
        )
    newOffset <- getFileSize event.eventPath
    atomically $ modifyTVar' offsetMap (insert event.eventPath newOffset)

regexWatcher :: TBQueue NotifyEvent -> OffsetMap -> RegexActionConfig -> Action
regexWatcher eventChannel offsetMap config =
    Action
        { predicate = (liftA2 (&&) newOrModifyFile (filepathMatch config.filepathRegex))
        , action = regexAction offsetMap config eventChannel
        }

type LastSeenMap = TVar (Map FilePath UTCTime)

touchFile :: LastSeenMap -> FS.Action
touchFile lastSeenMap event = atomically $ modifyTVar' lastSeenMap (insert event.eventPath event.eventTime)

staleFileUpdate :: LastSeenMap -> StaleFileConfig -> Action
staleFileUpdate lastSeenMap config =
    Action
        { predicate = (liftA2 (&&) (anyEventMatch eventTypes) (filepathMatch config.filepathRegex))
        , action = touchFile lastSeenMap
        }
  where
    eventTypes = [Create, Modify, ModifyAttribute]

staleFileChecker :: TBQueue NotifyEvent -> LastSeenMap -> StaleFileConfig -> IO Void
staleFileChecker eventChannel lastSeenMap config = forever $ do
    now <- getCurrentTime
    let flagTimeout = addUTCTime (negate config.timeout) now -- now - timeout
    atomically $ do
        stale <- (keys . Map.filter (<= flagTimeout)) <$> readTVar lastSeenMap
        mapM_ (writeTBQueue eventChannel . (StaleEvent config now)) stale
    threadDelay 60_000_000 -- one minute

eventAction :: TBQueue NotifyEvent -> EventActionConfig -> FS.Action
eventAction eventChannel config event = atomically (writeTBQueue eventChannel (FileEvent config event))

eventWatcher :: TBQueue NotifyEvent -> EventActionConfig -> Action
eventWatcher eventChannel config =
    Action
        { predicate = (liftA2 (&&) (anyEventMatch config.eventTypes) (filepathMatch config.filepathRegex))
        , action = (eventAction eventChannel config)
        }
