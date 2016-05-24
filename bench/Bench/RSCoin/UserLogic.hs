{-# LANGUAGE PatternGuards #-}

module Bench.RSCoin.UserLogic
        ( benchUserTransactions
        , initializeBank
        , initializeUser
        , userThread
        ) where


import           Control.Monad              (forM_, when)
import           Control.Monad.Catch        (bracket)
import           Control.Monad.Trans        (liftIO)
import           Data.Acid                  (createCheckpoint)
import           Data.Acid.Advanced         (query')
import           Data.Int                   (Int64)
import           Formatting                 (int, sformat, (%))
import           System.FilePath            ((</>))

import           Serokell.Util              (indexModulo)

import           RSCoin.Core                (Coin (..), bankSecretKey)

import           RSCoin.Timed               (MsgPackRpc, runRealMode)
import qualified RSCoin.User.AcidState      as A
import           RSCoin.User.Operations     (formTransactionRetry,
                                             updateBlockchain)
import           RSCoin.User.Wallet         (UserAddress, toAddress)

import           Bench.RSCoin.FilePathUtils (dbFormatPath)
import           Bench.RSCoin.Logging       (logDebug, logInfo)

transactionNum :: Int64
transactionNum = 1000

userThread :: (Int64 -> A.RSCoinUserState -> MsgPackRpc a) -> Int64 -> IO a
userThread userAction userId = runRealMode $ bracket
    (liftIO A.openMemState)
    (\userState -> liftIO $ do
        A.closeState userState
    )
    (userAction userId)

queryMyAddress :: A.RSCoinUserState -> MsgPackRpc UserAddress
queryMyAddress userState = head <$> query' userState A.GetAllAddresses

-- | Create user with 1 address and return it.
initializeUser :: Int64 -> A.RSCoinUserState -> MsgPackRpc UserAddress
initializeUser userId userState = do
    let userAddressesNumber = 1
    logDebug $ sformat ("Initializing user " % int % "…") userId
    A.initState userState userAddressesNumber Nothing
    queryMyAddress userState <*
        logDebug (sformat ("Initialized user " % int % "…") userId)

executeTransaction :: A.RSCoinUserState -> Int64 -> UserAddress -> MsgPackRpc ()
executeTransaction userState coinAmount addrToSend = do
    () <$ updateBlockchain userState False
    formTransactionRetry
        maxBound
        userState
        False
        inputMoneyInfo
        (toAddress addrToSend)
        outputMoney
  where
    outputMoney = Coin coinAmount
    inputMoneyInfo = [(1, outputMoney)]

-- | Create user in `bankMode` and send `transactionNum` coins to
-- every user from list.
initializeBank :: [UserAddress] -> A.RSCoinUserState -> MsgPackRpc ()
initializeBank userAddresses bankUserState = do
    let additionalBankAddreses = 0
    logDebug "Before initStateBank"
    A.initStateBank bankUserState additionalBankAddreses bankSecretKey
    logDebug "After initStateBank"
    forM_ userAddresses $ executeTransaction bankUserState transactionNum
    logDebug "Sent initial coins from bank to users"

-- | Start user with provided addresses of other users and do
-- `transactionNum` transactions.
benchUserTransactions :: [UserAddress] -> Int64 -> A.RSCoinUserState -> MsgPackRpc ()
benchUserTransactions allAddresses userId userState = do
    myAddress <- queryMyAddress userState
    let otherAddresses = filter (/= myAddress) allAddresses
        loggingStep = transactionNum `div` 5
    forM_ [0 .. transactionNum - 1] $
        \i ->
             do when (i `mod` loggingStep == 0) $
                    logInfo $
                    sformat
                        ("User " % int % " has executed " % int %
                         " transactions")
                        userId
                        i
                executeTransaction userState 1 . (otherAddresses `indexModulo`) $ i
