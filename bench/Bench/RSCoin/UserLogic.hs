module Bench.RSCoin.UserLogic
        ( benchUserTransactions
        , initializeBank
        , initializeUser
        , userThread
        ) where

import           Bench.RSCoin.FilePathUtils (dbFormatPath)

import           Control.Monad              (forM_)
import           Control.Monad.Catch        (MonadCatch, MonadThrow, bracket, catch, throwM)
import           Control.Monad.Trans        (liftIO)
import           Data.Acid                  (createCheckpoint, query)
import           Data.Int                   (Int64)

import           RSCoin.Core                (Coin (..), bankSecretKey)
import           RSCoin.Mintette            (MintetteError (MEInactive))

import qualified RSCoin.User.AcidState      as A
import           RSCoin.User.Operations     (formTransaction, updateBlockchain)
import qualified RSCoin.User.Wallet         as W

import           RSCoin.Timed               (MsgPackRpc, runRealMode)

import           System.FilePath            ((</>))

import Debug.Trace (trace)

transactionNum :: Int64
transactionNum = 100

userThread :: FilePath -> (A.RSCoinUserState -> MsgPackRpc a) -> Int64 -> IO a
userThread benchDir userAction userId = runRealMode $ bracket
    (liftIO $ A.openState $ benchDir </> dbFormatPath "wallet-db" userId)
    (\userState -> liftIO $ do
        createCheckpoint userState
        A.closeState userState
    )
    userAction

queryMyAddress :: A.RSCoinUserState -> MsgPackRpc W.UserAddress
queryMyAddress userState = do
    allAddresess <- liftIO $ query userState A.GetAllAddresses
    return $ head allAddresess

-- | Create user with 1 address and return it.
initializeUser :: A.RSCoinUserState -> MsgPackRpc W.UserAddress
initializeUser userState = do
    let userAddressesNumber = 1
    A.initState userState userAddressesNumber Nothing
    queryMyAddress userState

executeTransaction :: A.RSCoinUserState -> Int64 -> W.UserAddress -> MsgPackRpc ()
executeTransaction userState coinAmount addrToSend = do
    let outputMoney    = Coin coinAmount
    let inputMoneyInfo = [(1, outputMoney)]
    let transactionAction = do {
                            ; _ <- updateBlockchain userState False
                            ; formTransaction userState inputMoneyInfo (W.toAddress addrToSend) outputMoney
                            }
    inactiveHandler transactionAction
  where
    inactiveHandler :: (MonadCatch m, MonadThrow m) => m () -> m ()
    inactiveHandler transactionAction = transactionAction `catch` repeatIfInactive transactionAction

    repeatIfInactive :: (MonadCatch m, MonadThrow m) => m () -> MintetteError -> m ()
    repeatIfInactive transactionAction MEInactive = trace "succesful catch!" $ inactiveHandler transactionAction
    repeatIfInactive _                 someError  = trace "WAAAAAAAAAAAAAT?" $ throwM someError

-- | Create user in `bankMode` and send 1000 coins to every user from list.
initializeBank :: [W.UserAddress] -> A.RSCoinUserState -> MsgPackRpc ()
initializeBank userAddresses bankUserState = do
    let additionalBankAddreses = 0
    A.initStateBank bankUserState additionalBankAddreses bankSecretKey
    forM_ userAddresses $ executeTransaction bankUserState transactionNum

-- | Start user with provided addresses of other users and do 1000 transactions.
benchUserTransactions :: [W.UserAddress] -> A.RSCoinUserState -> MsgPackRpc ()
benchUserTransactions allAddresses userState = do
    myAddress         <- queryMyAddress userState

    let otherAddresses = filter (/= myAddress) allAddresses
    let numberOfUsers  = length otherAddresses
    forM_ [0..transactionNum - 1] $ \i -> do
        let userAddrToSend = otherAddresses !! (fromIntegral i `mod` numberOfUsers)
        executeTransaction userState 1 userAddrToSend