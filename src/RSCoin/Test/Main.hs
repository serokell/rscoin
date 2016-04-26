{-#LANGUAGE ScopedTypeVariables  #-}

module Main where

import           Prelude                 hiding (log)
import           Control.Monad           (forM_)
import           Control.Monad.Trans     (lift, liftIO, MonadIO)
import           Data.Default            (def)
import           System.Random           (mkStdGen) 
import           Control.Monad.Random.Class (getRandomR)

import           RSCoin.Test.MonadTimed  (wait, invoke, schedule, now, fork
                                         , work
                                         , at, after, for, till, during 
                                         , sec, minute, sec'
                                         , MonadTimed, runTimedIO_, localTime)
import           RSCoin.Test.Timed       (runTimedT)
import           RSCoin.Test.MonadRpc 
import           RSCoin.Test.PureRpc
import           Control.Monad.Catch

import           Network.MessagePack.Server (ServerT)

main :: IO ()
main  =  sayHelloIO

-- * Timed

sayHelloIO :: IO ()
sayHelloIO  =  runTimedIO_ sayHello

sayHelloPure :: IO ()
sayHelloPure  =  runTimedT sayHello

sayHello :: (MonadIO m, MonadTimed m) => m ()
sayHello  =  do
    invoke    now          $ log "Hello"
    invoke   (at    1 sec) $ log "It's 1 second now"

    log                          "Fork point"
    schedule (after 5 sec) $ log "5 more seconds passed"
    invoke   (after 2 sec) $ log "2 more seconds passed"

    wait     (for   5 sec) 
    log "Waited for 5 sec, now 8"
    wait     (till 10 sec) 
    log "Waited till 10-sec point"

    schedule (at    2 sec 1 minute) $ log "Aha!"
 
log :: (MonadIO m, MonadTimed m) => String -> m ()
log msg  =  do
    seconds <- time
    liftIO $ putStrLn $ mconcat $ ["[", show seconds, "s] ", msg]
  where
    time :: MonadTimed m => m Double
    time  =  ( / 1000000) . fromIntegral <$> localTime 

interruptedLol :: (MonadTimed m, MonadIO m) => m ()
interruptedLol  =  do
    work (during 5000000) tempLol

tempLol :: (MonadIO m, MonadTimed m) => m ()
tempLol  =  do
    liftIO $ putStrLn "Lol!"
    wait (for 2 sec)
    tempLol
    

-- * Rpc

rpcIO :: IO ()
rpcIO  =  runTimedIO_ . runMsgPackRpc $ handshake

rpcPure :: IO ()
rpcPure  =  rpcSeed 0

rpcSeed :: Int -> IO ()
rpcSeed seed  =  runPureRpc (mkStdGen seed) delays $ handshake

handshake :: (MonadRpc m, MonadTimed m, MonadIO m) => m ()
handshake  =  do
    let resp = response
    restrict $ resp 5
    work (during 5000000) $ serve 2222 [method "lol" resp]

    forM_ [1..3] $ \i ->
        schedule (at 1 sec) $ do
            forM_ [1..3] $ \j -> 
                schedule (at 2 sec) $ do
                    let a = i * 3 + j
                    log $ "Q" ++ show a
                    res <- execClient ("127.0.0.1", 2222) $ request a
                    log $ "A" ++ show res

restrict :: Monad m => ServerT m a -> m ()
restrict _  =  return ()

response :: (MonadRpc m, MonadTimed m, MonadIO m) => Int -> ServerT m Int
response k  =  do
    liftIO $ putStrLn $ "R" ++ show k
    lift $ wait $ for 0.4 sec'
    return k

request :: Int -> Client Int
request  =  call "lol" 

delays :: Delays
delays  =  Delays d
  where
    d _ _  =  Just <$> getRandomR (10, 1000)
