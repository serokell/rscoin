{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.RSCoin.Timed.ExceptionSpec
       ( spec
       ) where

import           Control.Exception.Base      (AsyncException(ThreadKilled), 
                                              ArithException(Overflow))
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad               (void)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO, 
                                              writeTVar, modifyTVar)
import           Control.Monad.Catch         (MonadCatch, throwM, catch,
                                              catchAll)
import           Control.Monad.Trans         (MonadIO, liftIO)
import           Control.Concurrent.STM      (atomically)
import           Data.Default                (def)
import           Data.Maybe                  (isJust)
import           Test.Hspec                  (Spec, describe)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (Property, Arbitrary (..),
                                              NonNegative (..))
import           Test.QuickCheck.Property    (ioProperty, Result(reason), 
                                              failed, succeeded, exception)
import           Test.QuickCheck.Monadic     (assert, monadicIO)
import           System.Random               (StdGen, mkStdGen)
import           Control.Monad.Random.Class  (getRandomR)

import           RSCoin.Timed                (MicroSeconds, sec, for,
                                              wait, runEmulationMode,
                                              Delays (..), fork, WorkMode, 
                                              PureRpc, invoke, after)

import           RSCoin.User.Error           (UserError (InputProcessingError))

spec :: Spec
spec =
    describe "WorkMode" $ do
        describe "error" $ do
            prop "should abort the execution"
                exceptionShouldAbortExecution
            prop "caught nicely"
                excCaught
            prop "exception from main thread caught nicely outside of monad"
                excCaughtOutside
            prop "proper catch order (catch inside catch)"
                excCatchOrder
            prop "(wait + throw) - exception doesn't get lost"
                excWaitThrow
            prop "(wait + throw) in forked thread - exception doesn't get lost"
                excWaitThrowForked
            prop "catch doesn't get handle future exceptions"
                excCatchScope
            prop "different exceptions, catch inner"
                excDiffCatchInner
            prop "different exceptions, catch outer"
                excDiffCatchOuter
        describe "async error" $
            prop "shouldn't abort the execution"
                asyncExceptionShouldntAbortExecution

exceptionShouldAbortExecution
    :: StdGen
    -> NonNegative MicroSeconds
    -> Property
exceptionShouldAbortExecution std (getNonNegative -> t) =
    monadicIO $ do
        var <- liftIO $ newTVarIO (0 :: Int)
        liftIO $ runEmulationMode std delays' $
            fork $ do
                liftIO $ atomically $ writeTVar var 1
                wait $ for t sec
                void $ throwM $ InputProcessingError "Error"
                liftIO $ atomically $ writeTVar var 2
        res <- liftIO $ readTVarIO var
        assert $ res == 1

asyncExceptionShouldntAbortExecution
    :: StdGen
    -> NonNegative MicroSeconds
    -> NonNegative MicroSeconds
    -> Property
asyncExceptionShouldntAbortExecution std (getNonNegative -> t1) (getNonNegative -> t2) =
    monadicIO $ do
        var <- liftIO $ newTVarIO (0 :: Int)
        liftIO $ runEmulationMode std delays' $ do
            liftIO $ atomically $ writeTVar var 1
            fork $ do
                wait $ for t2 sec
                throwM $ InputProcessingError "Error"
            wait $ for t1 sec
            liftIO $ atomically $ writeTVar var 2
        res <- liftIO $ readTVarIO var
        assert $ res == 2

excCaught
    :: StdGen
    -> Property
excCaught seed = 
    ioProperty . inSandbox . withCheckPoints $ 
        \checkPoint -> runEmu seed $ 
            let act   = throwM ThreadKilled >> checkPoint (-1)
                hnd _ = checkPoint 1
            in  act `catchAll` hnd

excCaughtOutside
    :: StdGen
    -> Property
excCaughtOutside seed = 
    ioProperty . inSandbox . withCheckPoints $ 
        \checkPoint -> 
            let act = runEmu seed (throwM ThreadKilled) >> checkPoint (-1)
                hnd _ = checkPoint 1
            in  act `catchAll` hnd
    
excWaitThrow
    :: StdGen
    -> Property
excWaitThrow seed = 
    ioProperty . inSandbox . withCheckPoints $ 
        \checkPoint -> runEmu seed $ 
            let act = do
                    wait (for 1 sec)
                    throwM ThreadKilled 
                hnd _ = checkPoint 1
            in  do
                    act `catchAll` hnd
                    checkPoint 2

excWaitThrowForked
    :: StdGen
    -> Property
excWaitThrowForked seed = 
    ioProperty . inSandbox . withCheckPoints $ 
        \checkPoint -> runEmu seed $ 
            let act = do
                    wait (for 1 sec)
                    throwM ThreadKilled 
                hnd _ = checkPoint 1
            in  do
                    fork $ act `catchAll` hnd
                    invoke (after 1 sec) $ checkPoint 2

excCatchOrder
    :: StdGen
    -> Property
excCatchOrder seed = 
    ioProperty . inSandbox . withCheckPoints $ 
        \checkPoint -> runEmu seed $ 
            let act    = throwM ThreadKilled 
                hnd1 _ = checkPoint 1
                hnd2 _ = checkPoint (-1)
            in  do  act `catchAll` hnd1 `catchAll` hnd2
                    checkPoint 2

excCatchScope
    :: StdGen
    -> Property
excCatchScope seed = 
    ioProperty . inSandbox . withCheckPoints $ 
        \checkPoint -> runEmu seed $ 
            let act1 = checkPoint 1 `catchAll` const (checkPoint $ -1)
                act2 = act1 >> throwM ThreadKilled
            in  do
                act2 `catchAll` const (checkPoint 2)
                checkPoint 3

excDiffCatchInner
    :: StdGen
    -> Property
excDiffCatchInner seed = 
    ioProperty . inSandbox . withCheckPoints $ 
        \checkPoint -> runEmu seed $ 
            let act = throwM ThreadKilled 
                hnd1 (e :: AsyncException) = checkPoint 1
                hnd2 (e :: ArithException) = checkPoint (-1)
            in  do
                act `catch` hnd1 `catch` hnd2
                checkPoint 2

excDiffCatchOuter
    :: StdGen
    -> Property
excDiffCatchOuter seed = 
    ioProperty . inSandbox . withCheckPoints $ 
        \checkPoint -> runEmu seed $ 
            let act = throwM Overflow
                hnd1 (e :: AsyncException) = checkPoint (-1)
                hnd2 (e :: ArithException) = checkPoint 1
            in  do
                act `catch` hnd1 `catch` hnd2
                checkPoint 2


-- TODO: this is kind of odd
instance Arbitrary StdGen where
    arbitrary = mkStdGen <$> arbitrary

-- FIXME: use arbitrary instead of StdGen
delays' :: Delays
delays' = Delays d
  where
    d _ _ = Just <$> getRandomR (10, 1000)


runEmu :: StdGen -> PureRpc IO () -> IO ()
runEmu seed = runEmulationMode seed def

-- Principle of checkpoints: every checkpoint has it's id
-- Checkpoints should be visited in according order: 1, 2, 3 ...
newtype CheckPoints = CP { getCP :: TVar (Either String Int) }

initCheckPoints :: MonadIO m => m CheckPoints
initCheckPoints = fmap CP $ liftIO $ newTVarIO $ Right 0

visitCheckPoint :: MonadIO m => CheckPoints -> Int -> m ()
visitCheckPoint cp curId = liftIO $ atomically $ modifyTVar (getCP cp) $
    \wasId -> 
        if wasId == Right (curId - 1)
            then Right curId
            else Left $ either id (showError curId) wasId 
  where
    showError cur was = mconcat 
        ["Wrong chechpoint. Expected "
        , show (was + 1)
        , ", but visited "
        , show cur
        ]
    
assertCheckPoints :: MonadIO m => CheckPoints -> m Result
assertCheckPoints = fmap mkRes . liftIO . readTVarIO . getCP
  where
    mkRes (Left msg) = failed { reason = msg }
    mkRes (Right _)  = succeeded

withCheckPoints :: MonadIO m => ((Int -> m ()) -> IO a) -> IO Result
withCheckPoints act = do
    cp <- initCheckPoints 
    _  <- act $ liftIO . visitCheckPoint cp
    assertCheckPoints cp

inSandbox :: MonadCatch m => m Result -> m Result
inSandbox = flip catchAll $ return . exception "Unexpected exception"
