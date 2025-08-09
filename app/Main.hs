module Main where

import Action (
    Action (..),
    NotifyEvent (..),
    eventWatcher,
    regexWatcher,
    staleFileChecker,
    staleFileUpdate,
 )
import Config (
    AppConfig (
        eventActions,
        queueLength,
        regexActions,
        staleFileActions
    ),
    EventActionConfig (cmd, directory, name),
    RegexActionConfig (cmd, directory, name),
    StaleFileConfig (cmd, directory, name, timeout),
    directories,
    parseConfigFromFile,
 )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM.TBQueue (newTBQueue, readTBQueue)
import Control.Exception (handle)
import Data.Map qualified as Map
import Options.Applicative (
    Parser,
    ParserInfo,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    metavar,
    progDesc,
    strArgument,
 )
import Predicate (EventType (..), getEventType)
import Relude.Extra.Map (lookupDefault)
import System.Environment (getEnvironment)
import System.FSNotify (Event (eventPath, eventTime), watchDir, withManager)
import System.FSNotify qualified as FS
import System.Process (
    CreateProcess (env, std_err, std_out),
    StdStream (NoStream),
    createProcess,
    shell,
    waitForProcess,
 )

fireAndForget :: [(String, String)] -> String -> IO ()
fireAndForget callBackEnvironment cmd = do
    userEnvironment <- getEnvironment
    void $
        handle (\(_ :: SomeException) -> pure ()) $
            do
                (_, _, _, ph) <-
                    createProcess
                        ( (shell cmd)
                            { env = Just (callBackEnvironment <> userEnvironment)
                            , std_out = NoStream -- Discard stdout
                            , std_err = NoStream -- Discard stderr
                            }
                        )
                void $ waitForProcess ph

withAsyncs :: [IO a] -> IO b -> IO b
withAsyncs asyncs action = foldr (\async form -> withAsync async (\_ -> form)) action asyncs

handleEvent :: NotifyEvent -> IO ()
handleEvent (MatchEvent config inotifyEvent match) = fireAndForget environment config.cmd
  where
    environment :: [(String, String)]
    environment =
        [ ("NAME", toString config.name)
        , ("FILE", eventPath inotifyEvent)
        , ("DIRECTORY", config.directory)
        , ("TIMESTAMP", show (eventTime inotifyEvent))
        , ("MATCH", decodeUtf8 match)
        ]
handleEvent (FileEvent config inotifyEvent) = fireAndForget environment config.cmd
  where
    environment :: [(String, String)]
    environment =
        [ ("NAME", toString config.name)
        , ("FILE", eventPath inotifyEvent)
        , ("DIRECTORY", config.directory)
        , ("TIMESTAMP", show (eventTime inotifyEvent))
        , ("KIND", showEvent (getEventType inotifyEvent))
        ]
    showEvent :: EventType -> String
    showEvent Create = "create"
    showEvent Modify = "modify"
    showEvent DeleteDirectory = "delete_directory"
    showEvent UnknownEvent = "unknown"
    showEvent ModifyAttribute = "attribute"
    showEvent Remove = "remove"
    showEvent Close = "close"
handleEvent (StaleEvent config timestamp fp) = fireAndForget environment config.cmd
  where
    environment :: [(String, String)]
    environment =
        [ ("NAME", toString config.name)
        , ("FILE", fp)
        , ("DIRECTORY", config.directory)
        , ("TIMESTAMP", show timestamp)
        , ("TIMEOUT", show config.timeout)
        ]

route :: Map FilePath [Action] -> FilePath -> FS.Action
route actions directory event =
    mapM_
        ( \action ->
            action.action event
        )
        . filter (\action -> action.predicate event)
        $ lookupDefault mempty directory actions

startSidecar :: AppConfig -> IO ()
startSidecar config = do
    eventChannel <- atomically (newTBQueue config.queueLength)
    offsetMaps <- replicateM (length config.regexActions) (newTVarIO mempty)
    lastSeenMap <- newTVarIO mempty
    withAsync (forever $ atomically (readTBQueue eventChannel) >>= handleEvent) $ \_ -> do
        let actions =
                Map.unionsWith (<>) $
                    (zipWith (\offsetMap cfg -> Map.singleton cfg.directory [regexWatcher eventChannel offsetMap cfg]) offsetMaps config.regexActions)
                        <> (map (\cfg -> Map.singleton cfg.directory [staleFileUpdate lastSeenMap cfg]) config.staleFileActions)
                        <> (map (\cfg -> Map.singleton cfg.directory [eventWatcher eventChannel cfg]) config.eventActions)

        withManager $ \mgr -> do
            mapM_ (\dir -> watchDir mgr dir (const True) (route actions dir)) (directories config)
            unless (null config.staleFileActions) $ do
                withAsyncs (map (staleFileChecker eventChannel lastSeenMap) config.staleFileActions) $ do
                    forever (threadDelay maxBound)
            forever (threadDelay maxBound)

-- CLI Options data type
data CliOptions = CliOptions
    { configFile :: String
    }
    deriving (Show)

-- Parser for individual options
cliOptions :: Parser CliOptions
cliOptions =
    CliOptions
        <$> strArgument (metavar "FILE" <> help "Configuration file path")

-- Program info
opts :: ParserInfo CliOptions
opts =
    info
        (cliOptions <**> helper)
        ( fullDesc
            <> progDesc "File system watcher with configurable actions designed for monitoring long-running processes."
            <> header "sidecar - a process monitoring tool"
        )

-- Updated main function
main :: IO ()
main = do
    options <- execParser opts
    configResult <- parseConfigFromFile (options.configFile)
    case configResult of
        Right config -> startSidecar config
        Left err -> putTextLn err
