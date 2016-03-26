{-# LANGUAGE ScopedTypeVariables #-}

-- | Worker that handles end of period.

module RSCoin.Bank.Worker
       ( runWorker
       ) where

import           Control.Concurrent    (forkFinally, threadDelay)
import           Control.Exception     (SomeException, try)
import           Control.Monad         (void)
import           Data.Acid             (createCheckpoint, query, update)
import           Data.Time.Units       (toMicroseconds)

import           RSCoin.Core           (Mintette, PeriodId, PeriodResult,
                                        SecretKey, periodDelta)

import           RSCoin.Bank.AcidState (GetMintettes (..), GetPeriodId (..),
                                        StartNewPeriod (..), State)

-- | Start worker which runs appropriate action when a period finishes
runWorker :: SecretKey -> State -> IO ()
runWorker sk st =
    foreverE $
    do onPeriodFinished sk st
       threadDelay (fromIntegral $ toMicroseconds periodDelta)
  where
    foreverE f = void $ forkFinally f $ handler $ foreverE f
    -- TODO: use logging system once we have one
    handler f (Left (e :: SomeException)) = do
        putStrLn $
            "Error occurred in worker, restarting in 1 minute: " ++ show e
        threadDelay $ 1 * 60 * 1000 * 1000
        f
    handler f (Right _) = f

onPeriodFinished :: SecretKey -> State -> IO ()
onPeriodFinished sk st = do
    mintettes <- query st GetMintettes
    pId <- query st GetPeriodId
    -- Mintettes list is empty before the first period, so we'll simply
    -- get [] here in this case (and it's fine).
    periodResults <- mapM (handleError . flip sendPeriodFinished pId) mintettes
    update st $ StartNewPeriod sk periodResults
    createCheckpoint st
  where
    handleError action = do
        either onError (return . Just) =<< try action
    -- TODO: catching appropriate exception according to protocol implementation
    onError (e :: SomeException) = do
        putStrLn $ "Error occurred: " ++ show e
        return Nothing

sendPeriodFinished :: Mintette -> PeriodId -> IO PeriodResult
sendPeriodFinished = undefined  -- it depends on protocol