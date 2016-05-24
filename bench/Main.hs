{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Concurrent.Async   (forConcurrently)
import           Control.Monad              (forM_, replicateM, void)
import           Data.Int                   (Int64)
import           Data.Maybe                 (fromMaybe)
import           Data.Time.Clock            (NominalDiffTime, diffUTCTime,
                                             getCurrentTime)
import           Data.Time.Units            (toMicroseconds)
import           Formatting                 (build, sformat, shown, (%))

-- workaround to make stylish-haskell work :(
import           Options.Generic

import           System.IO.Temp             (withSystemTempDirectory)

import qualified RSCoin.Bank                as B
import           RSCoin.Core                (PublicKey, SecretKey,
                                             Severity (..), initLogging, keyGen)
import qualified RSCoin.User                as U
import           RSCoin.User.Wallet         (UserAddress)

import           Bench.RSCoin.FilePathUtils (tempBenchDirectory)
import           Bench.RSCoin.InfraThreads  (addMintette, bankThread,
                                             defaultBenchPeriod, mintetteThread)
import           Bench.RSCoin.Logging       (initBenchLogger, logInfo)
import           Bench.RSCoin.UserLogic     (benchUserTransactions,
                                             initializeBank, initializeUser,
                                             userThread)

data BenchOptions = BenchOptions
    { users         :: Int            <?> "number of users"
    , mintettes     :: Int            <?> "number of mintettes"
    , severity      :: Maybe Severity <?> "severity for global logger"
    , benchSeverity :: Maybe Severity <?> "severity for bench logger"
    } deriving (Generic, Show)

instance ParseField  Severity
instance ParseFields Severity
instance ParseRecord Severity
instance ParseRecord BenchOptions

type KeyPairList = [(SecretKey, PublicKey)]

generateMintetteKeys :: Int -> IO KeyPairList
generateMintetteKeys n = replicateM n keyGen

runMintettes :: B.State -> KeyPairList -> IO ()
runMintettes bankState secretKeys
    = forM_ (zip [1..] secretKeys) $ \(mintetteId, (secretKey, publicKey)) -> do
        addMintette mintetteId bankState publicKey
        void $ forkIO $ mintetteThread mintetteId secretKey

establishMintettes :: B.State -> Int -> IO ()
establishMintettes bankState mintettesNumber = do
    keyPairs <- generateMintetteKeys mintettesNumber
    runMintettes bankState keyPairs
    logInfo $ sformat ("Running " % build % " mintettes…") mintettesNumber
    threadDelay $ 5 * 10 ^ (6 :: Int)

establishBank :: B.State -> IO ()
establishBank st = do
    _ <- forkIO $ bankThread st
    logInfo "Running bank..."
    threadDelay (1 * 10 ^ (6 :: Int))

initializeUsers :: [Int64] -> IO [(UserAddress, U.RSCoinUserState)]
initializeUsers userIds = do
    logInfo $ sformat ("Initializing " % build % " users…") $ length userIds
    mapM initializeUser userIds

initializeSuperUser :: [UserAddress] -> IO ()
initializeSuperUser userAddresses = do
    -- give money to all users
    let bankId = 0
    logInfo "Initializaing user in bankMode…"
    userThread (const $ initializeBank userAddresses) bankId
    logInfo "Initialized user in bankMode, now waiting for the end of the period…"
    threadDelay $ fromInteger $ toMicroseconds (defaultBenchPeriod + 1)

runTransactions :: [UserAddress]
                -> [U.RSCoinUserState]
                -> [Int64]
                -> IO NominalDiffTime
runTransactions userAddresses userStates userIds = do
    let benchUserAction = benchUserTransactions userAddresses

    logInfo "Running transactions…"
    timeBefore <- getCurrentTime
    _ <- forConcurrently (zip userStates userIds) benchUserAction
    timeAfter <- getCurrentTime

    return $ timeAfter `diffUTCTime` timeBefore

main :: IO ()
main = do
    BenchOptions{..} <- getRecord "rscoin-user-bench"
    let mintettesNumber = unHelpful mintettes
        userNumber      = unHelpful users
        globalSeverity  = fromMaybe Warning $ unHelpful severity
        bSeverity       = fromMaybe Debug   $ unHelpful benchSeverity
    initLogging globalSeverity
    initBenchLogger bSeverity

    bankState <- B.openMemState
    establishMintettes bankState mintettesNumber
    establishBank bankState

    let userIds    = [1 .. fromIntegral userNumber]
    (userAddresses, userStates) <- unzip <$> initializeUsers userIds
    initializeSuperUser userAddresses

    elapsedTime <- runTransactions userAddresses userStates userIds
    logInfo $ sformat ("Elapsed time: " % shown) elapsedTime
