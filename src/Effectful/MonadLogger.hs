{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Provides a 'Logger' effect and orphan instance for 'MonadLogger',
-- allowing @effectful@ to use 'MonadLogger' functions.
module Effectful.MonadLogger
  ( -- * Logger effect
    Logger (..),
    log,

    -- * Re-exports
    LogLevel (..),
    LogLine,
    LogSource,
    LogStr,
    ToLogStr (..),
    fromLogStr,

    -- * Running the 'Logger' effect
    runLogger,
    runLoggerStderr,
    runLoggerStdout,
    runLoggerChan,
    runLoggerFile,
    unChanLogger,

    -- ** Reinterpreting the 'Logger' effect
    withChannelLogger,
    filterLogger,

    -- ** Disabling logging
    runLoggerNoLogging,

    -- ** Pure logging
    runLoggerWriter,
    execLoggerWriter,

    -- * TH logging
    logDebug,
    logInfo,
    logWarn,
    logError,
    logOther,

    -- * TH logging of showable values
    logDebugSH,
    logInfoSH,
    logWarnSH,
    logErrorSH,
    logOtherSH,

    -- * TH logging with source
    logDebugS,
    logInfoS,
    logWarnS,
    logErrorS,
    logOtherS,

    -- * TH util
    liftLoc,

    -- * Non-TH logging
    logDebugN,
    logInfoN,
    logWarnN,
    logErrorN,
    logOtherN,

    -- * Non-TH logging with source
    logWithoutLoc,
    logDebugNS,
    logInfoNS,
    logWarnNS,
    logErrorNS,
    logOtherNS,

    -- * Callstack logging
    logDebugCS,
    logInfoCS,
    logWarnCS,
    logErrorCS,
    logOtherCS,

    -- * Utilities for defining your own loggers
    defaultLogStr,
    Loc (..),
    defaultLoc,
    defaultOutput,
  )
where

import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBChan as STM
import Control.Exception (onException)
import Control.Monad (forever, when)
import Control.Monad.Logger hiding
  ( defaultOutput,
    filterLogger,
    withChannelLogger,
  )
import qualified Control.Monad.Logger as Logger
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Monoid (Endo (..))
import Effectful (Eff, Effect, IOE, liftIO, withRunInIO, (:>))
import Effectful.Dispatch.Dynamic (interpose, interpret, reinterpret)
import Effectful.TH (makeEffect)
import Effectful.Writer.Static.Shared (Writer, runWriter, tell)
import System.IO
  ( BufferMode (..),
    Handle,
    IOMode (..),
    hSetBuffering,
    stderr,
    stdout,
    withFile,
  )
import Prelude hiding (log)

data Logger :: Effect where
  Log :: (ToLogStr msg) => Loc -> LogSource -> LogLevel -> msg -> Logger m ()

$(makeEffect ''Logger)

-- | Analogue to 'Control.Monad.Logger.runLoggingT'
--
-- @since 0.1.0.0
runLogger ::
  (Loc -> LogSource -> LogLevel -> LogStr -> Eff es ()) ->
  Eff (Logger ': es) a ->
  Eff es a
runLogger f = interpret $ \_ (Log loc source level msg) ->
  f loc source level $ toLogStr msg

-- | Analogue to 'Control.Monad.Logger.runStderrLoggingT'
--
-- @since 0.1.0.0
runLoggerStderr :: (IOE :> es) => Eff (Logger ': es) a -> Eff es a
runLoggerStderr = runLogger $ defaultOutput stderr

-- | Analogue to 'Control.Monad.Logger.runStdoutLoggingT'
--
-- @since 0.1.0.0
runLoggerStdout :: (IOE :> es) => Eff (Logger ': es) a -> Eff es a
runLoggerStdout = runLogger $ defaultOutput stdout

-- | Analogue to 'Control.Monad.Logger.runChanLoggingT'
--
-- @since 0.1.0.0
runLoggerChan :: (IOE :> es) => Chan LogLine -> Eff (Logger ': es) a -> Eff es a
runLoggerChan chan = runLogger sink
  where
    sink loc source level msg =
      liftIO $ writeChan chan (loc, source, level, msg)

-- | Analogue to 'Control.Monad.Logger.runFileLoggingT'
--
-- @since 0.1.0.0
runLoggerFile :: (IOE :> es) => FilePath -> Eff (Logger ': es) a -> Eff es a
runLoggerFile path m = withRunInIO $ \runInIO ->
  withFile path AppendMode $ \h -> do
    hSetBuffering h LineBuffering
    runInIO $ runLogger (defaultOutput h) m

-- | Analogue to 'Control.Monad.Logger.unChanLoggingT'
--
-- @since 0.1.0.0
unChanLogger :: (Logger :> es, IOE :> es) => Chan LogLine -> Eff es void
unChanLogger chan = forever $ do
  (loc, source, level, msg) <- liftIO $ readChan chan
  log loc source level msg

-- | Analogue to 'Control.Monad.Logger.withChannelLogger'
--
-- @since 0.1.0.0
withChannelLogger ::
  forall es a. (Logger :> es, IOE :> es) => Int -> Eff es a -> Eff es a
withChannelLogger size m = do
  chan <- liftIO $ STM.newTBChanIO @LogLine size

  let logToChan :: Logger (Eff localEs) x -> Eff es x
      logToChan (Log loc source level msg) = liftIO . atomically $ do
        full <- STM.isFullTBChan chan
        when full . void $ STM.readTBChan chan
        STM.writeTBChan chan (loc, source, level, toLogStr msg)

      drainLogChan :: Eff es [LogLine]
      drainLogChan = liftIO $ atomically loop
        where
          loop = do
            empty <- STM.isEmptyTBChan chan
            if empty
              then pure []
              else do
                line <- STM.readTBChan chan
                (line :) <$> loop

      dumpLogChan :: Eff es ()
      dumpLogChan =
        drainLogChan
          >>= traverse_ (\(loc, source, level, msg) -> log loc source level msg)

  withRunInIO $ \runInIO ->
    runInIO (interpose (const logToChan) m) `onException` runInIO dumpLogChan

-- | Analogue to 'Control.Monad.Logger.filterLogger'
--
-- @since 0.1.0.0
filterLogger ::
  (Logger :> es) => (LogSource -> LogLevel -> Bool) -> Eff es a -> Eff es a
filterLogger f = interpose $ \_ (Log loc source level msg) ->
  when (f source level) $ log loc source level msg

-- | Analogue to 'Control.Monad.Logger.runNoLoggingT'
--
-- @since 0.1.0.0
runLoggerNoLogging :: Eff (Logger ': es) a -> Eff es a
runLoggerNoLogging = interpret $ \_ Log {} -> pure ()

-- | Analogue to 'Control.Monad.Logger.runWriterLoggingT'
--
-- @since 0.1.0.0
runLoggerWriter :: Eff (Logger ': es) a -> Eff es (a, [LogLine])
runLoggerWriter = reinterpret withWriter $ \_ (Log loc source level msg) ->
  tell $ Endo ((loc, source, level, toLogStr msg) :)
  where
    -- Use Endo [LogLine] in the writer to avoid O(n^2) appends
    withWriter :: Eff (Writer (Endo [LogLine]) ': es) a -> Eff es (a, [LogLine])
    withWriter m = do
      (a, linesEndo) <- runWriter m
      pure (a, linesEndo `appEndo` [])

-- | Analogue to 'Control.Monad.Logger.execWriterLoggingT'
--
-- @since 0.1.0.0
execLoggerWriter :: Eff (Logger ': es) a -> Eff es [LogLine]
execLoggerWriter = fmap snd . runLoggerWriter

-- | Analogue to 'Control.Monad.Logger.defaultOutput'
--
-- @since 0.1.0.0
defaultOutput ::
  (IOE :> es) => Handle -> Loc -> LogSource -> LogLevel -> LogStr -> Eff es ()
defaultOutput h loc source level msg =
  liftIO $ Logger.defaultOutput h loc source level msg

-- | Canonical orphan instance
--
-- @since 0.1.0.0
instance (Logger :> es) => MonadLogger (Eff es) where
  monadLoggerLog loc source level msg = log loc source level msg

-- | Canonical orphan instance
--
-- @since 0.1.0.0
instance (IOE :> es, Logger :> es) => MonadLoggerIO (Eff es) where
  askLoggerIO = do
    withRunInIO $ \runInIO -> pure $ \loc source level msg ->
      runInIO $ monadLoggerLog loc source level msg
