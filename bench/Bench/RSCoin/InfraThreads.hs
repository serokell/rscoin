module Bench.RSCoin.InfraThreads
        ( addMintette
        , bankThread
        , defaultBenchPeriod
        , mintetteThread
        ) where

import           Bench.RSCoin.FilePathUtils (dbFormatPath)

import           Control.Monad.Catch        (bracket)
import           Control.Monad.Trans        (liftIO)
import           Data.Acid                  (update)
import           Data.Time.Units            (Second)

import qualified RSCoin.Bank                as B
import           RSCoin.Core                (Mintette (Mintette), PublicKey,
                                             SecretKey, bankHost, bankSecretKey,
                                             defaultPort)
import qualified RSCoin.Mintette            as M

import           RSCoin.Timed               (MsgPackRpc, fork, runRealMode)

import           System.FilePath            ((</>))

defaultBenchPeriod :: Second
defaultBenchPeriod = 7

bankBracket :: FilePath -> (B.State -> MsgPackRpc a) -> IO a
bankBracket benchDir bankAction = runRealMode $ bracket
    (liftIO B.openMemState)
    (liftIO . B.closeState)
    bankAction

addMintette :: Int -> FilePath -> PublicKey -> IO ()
addMintette mintetteId benchDir publicKey = bankBracket benchDir $ \bankState -> liftIO $ do
    let mintette = Mintette bankHost (defaultPort + mintetteId)
    update bankState $ B.AddMintette mintette publicKey

bankThread :: FilePath -> IO ()
bankThread benchDir = bankBracket benchDir $ \bankState -> do
    _ <- fork $ B.runWorkerWithPeriod defaultBenchPeriod bankSecretKey bankState
    B.serve bankState

mintetteThread :: Int -> FilePath -> SecretKey -> IO ()
mintetteThread mintetteId benchDir secretKey = runRealMode $ bracket
    (liftIO M.openMemState)
    (liftIO . M.closeState) $
    \mintetteState -> do
        _ <- fork $ M.runWorker secretKey mintetteState
        M.serve (defaultPort + mintetteId) mintetteState secretKey
