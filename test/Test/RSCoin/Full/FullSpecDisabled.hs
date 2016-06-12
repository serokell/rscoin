{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | HSpec specification of full rscoin.

module Test.RSCoin.Full.FullSpec
       ( spec
       ) where

import           Control.Lens                    (view, views)
import           Data.List                       (nub)
import           Test.Hspec                      (Spec, before, describe)
import           Test.Hspec.QuickCheck           (prop)
import           Test.QuickCheck                 (Arbitrary (arbitrary),
                                                  NonEmptyList (NonEmpty))

import           RSCoin.Core                     (Severity (..), bankLoggerName,
                                                  genesisValue,
                                                  initLoggerByName, initLogging,
                                                  testingLoggerName)
import qualified RSCoin.User                     as U

import           Test.RSCoin.Core.Arbitrary      ()
import           Test.RSCoin.Full.Action         (UserAction (..), UserIndex,
                                                  getUser)
import           Test.RSCoin.Full.Context        (buser, state)
import           Test.RSCoin.Full.Property       (FullProperty, assertFP,
                                                  doActionFP, pickFP,
                                                  runTestEnvFP, runWorkModeFP)
import qualified Test.RSCoin.Full.UserOperations as UO

spec :: Spec
spec =
    before setupLogging $ do
        describe "test" $
            prop "test" test
        describe "Full RSCoin" $ do
            prop "if bank sends all coins to arbitrary address then it has 0 coins" prop_sendAll
            prop "all users have unique addresses" prop_uniqueAddresses

setupLogging :: IO ()
setupLogging = do
    initLogging Info
    initLoggerByName Info bankLoggerName
    initLoggerByName Info testingLoggerName

test :: FullProperty ()
test = assertFP True

prop_sendAll :: FullProperty ()
prop_sendAll = do
    buSt <- view $ buser . state
    amount <- getAmount buSt 1
    addr <- pickFP arbitrary
    doActionFP $ FormTransaction Nothing (NonEmpty [(1, 1)]) $ Left addr
    amount' <- getAmount buSt 1
    assertFP $ amount' == 0
    assertFP $ amount' - amount == genesisValue

prop_uniqueAddresses :: UserIndex -> FullProperty ()
prop_uniqueAddresses idx = do
    usr <- runTestEnvFP $ getUser idx
    assertFP . isUnique =<< runWorkModeFP (UO.getAllAddresses usr)
  where
    isUnique l = l == nub l

-- prop_sendLoopBack :: FullProperty ()
-- prop_sendLoopBack = do
--     buSt <- view $ buser . state
--     amount <- getAmount buSt 1
--     addr <- head <$> runWorkModeFP (U.getAllPublicAddresses buSt)
--     doActionFP $ FormTransaction Nothing (NonEmpty [(1, 50)]) $ Left addr
--     amount' <- getAmount buSt 1
--     assertFP $ amount' == amount

-- prop_send2inARow :: FullProperty ()
-- prop_send2inARow = do
--     buSt <- view $ buser . state
--     addrs <- runWorkModeFP $ U.getAllPublicAddresses buSt
--     amount1 <- getAmount buSt 1
--     amount2 <- getAmount buSt 2
--     amount3 <- getAmount buSt 3
--     doActionFP $ FormTransaction Nothing (NonEmpty [(1, 50)]) $ Left (addrs !! 2)
--     doActionFP $ FormTransaction Nothing (NonEmpty [(2, 50)]) $ Left (addrs !! 3)
--     amount1' <- getAmount buSt 1
--     amount2' <- getAmount buSt 2
--     amount3' <- getAmount buSt 3
--     assertFP $ amount1 - amount1' == 50
--     assertFP $ amount3' - amount3 == 50
--     assertFP $ amount2' == amount2

getAmount buSt i = runWorkModeFP $ U.getAmountByIndex buSt i
