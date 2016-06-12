{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Re-export RSCoin.Timed.*

module RSCoin.Timed
       ( module Exports
       , WorkMode
       , runRealMode
       , runRealModeLocal
       , runEmulationMode
       , runEmulationMode_
       ) where

import           RSCoin.Timed.Misc       as Exports
import           RSCoin.Timed.MonadRpc   as Exports
import           RSCoin.Timed.MonadTimed as Exports
import           RSCoin.Timed.PureRpc    as Exports
import           RSCoin.Timed.Timed      as Exports
import           RSCoin.Timed.TimedIO    as Exports

import           Control.Monad           (join)
import           Control.Monad.Catch     (MonadMask)
import           Control.Monad.Reader    (MonadReader, runReaderT)
import           Control.Monad.Trans     (MonadIO, liftIO)
import           Data.ByteString         (ByteString)
import           System.Random           (StdGen, getStdGen)

class (MonadTimed m, MonadRpc m, MonadIO m,
       MonadMask m, MonadReader BankSettings m) => WorkMode m where

instance (MonadTimed m, MonadRpc m, MonadIO m,
       MonadMask m, MonadReader BankSettings m) => WorkMode m

runRealMode :: ByteString -> MsgPackRpc a -> IO a
runRealMode bankHost rpcMonad = do
    settings <- emptySettings bankHost
    runTimedIO $ flip runReaderT settings $ runMsgPackRpc rpcMonad

runRealModeLocal :: MsgPackRpc a -> IO a
runRealModeLocal = runRealMode "127.0.0.1"

runEmulationMode :: MonadIO m => Maybe StdGen -> Delays -> PureRpc IO a -> m a
runEmulationMode genMaybe delays m =
    liftIO . join $ runPureRpc <$> getGen genMaybe <*> pure delays <*> pure m

runEmulationMode_ :: MonadIO m => Maybe StdGen -> Delays -> PureRpc IO () -> m ()
runEmulationMode_ genMaybe delays m =
    liftIO . join $ runPureRpc_ <$> getGen genMaybe <*> pure delays <*> pure m

getGen :: Maybe StdGen -> IO StdGen
getGen = maybe getStdGen pure
