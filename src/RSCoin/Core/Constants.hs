{-# LANGUAGE TemplateHaskell   #-}

-- | This module contains all constants in rscoin.

module RSCoin.Core.Constants
       ( defaultSecretKeyPath
       , defaultAccountsNumber
       , defaultPort
       , defaultBankHost
       , bankPort
       , defaultPeriodDelta
       , epochDelta
       , emissionHash
       , genesisAddress
       , genesisValue
       , periodReward
       , shardDivider
       , shardDelta
       , rpcTimeout
       , rpcConnectionNumber
       , bankSecretKey
       ) where

import           Data.Binary                (Binary)
import           Data.FileEmbed             (embedFile, makeRelativeToProject)
import           Data.Maybe                 (fromJust)
import           Data.String                (IsString)
import           Data.Time.Units            (Second)
import           Language.Haskell.TH.Syntax (Lift (lift))
import           System.Directory           (getHomeDirectory)
import           System.FilePath            ((</>))

import qualified RSCoin.Core.CompileConfig  as CC
import           RSCoin.Core.Crypto         (Hash, SecretKey, constructPublicKey,
                                             constructSecretKey, hash)
import           RSCoin.Core.Primitives     (Address (Address), Coin)

-- | Path used by default to read/write secret key.
defaultSecretKeyPath :: IO FilePath
defaultSecretKeyPath = (\h -> h </> ".rscoin" </> "key") <$> getHomeDirectory

-- | The default number of accounts (sk-pk pairs) generated with the
-- wallet (user part)
defaultAccountsNumber :: Int
defaultAccountsNumber = 5

-- | Default port used by applications.
defaultPort :: Num a => a
defaultPort = 3000

defaultBankHost :: IsString s => s
defaultBankHost = "127.0.0.1"

bankPort :: Num a => a
bankPort = defaultPort

defaultPeriodDelta :: Second
defaultPeriodDelta = 100

epochDelta :: Second
epochDelta = 5

emissionHash :: Binary t => t -> Hash
emissionHash a =
    hash ("This emission hash is needed for all generative" ++
          "transactions to be different" :: String, a)

-- | Special address used as output in genesis transaction
genesisAddress :: Address
genesisAddress =
    Address $
    fromJust $
    constructPublicKey "YblQ7+YCmxU/4InsOwSGH4Mm37zGjgy7CLrlWlnHdnM="

-- | This value is sent to genesisAddress in genesis transaction
genesisValue :: Coin
genesisValue = 100000000

-- | This value is allocated by Bank in the end of a period.
-- It's then sent distributed accross participating mintettes and Bank.
-- Ideally it should change over time, but let's make it simple.
periodReward :: Coin
periodReward = 1000

-- | The amount of mintettes divided my shardDivider equals to shard
-- size.
shardDivider :: Int
shardDivider = $(lift $ CC.shardDivider CC.readRSCoinConfig)

-- | The amount of mintettes to be added to shard size.
shardDelta :: Int
shardDelta = $(lift $ CC.shardDelta CC.readRSCoinConfig)

-- | Timeout for rpc calls in microsecons.
-- If timeout exceedes TimeoutError is thrown.
rpcTimeout :: Second
rpcTimeout = 5

-- | Maximum size of allowed open sockets in one time.
rpcConnectionNumber :: Integer
rpcConnectionNumber = 1000

-- | Bank's secret key which can be used to spend coins from genesis transaction.
-- It's needed only for tests/benchmarks.
bankSecretKey :: SecretKey
bankSecretKey =
    constructSecretKey $ $(makeRelativeToProject "rscoin-key" >>= embedFile)
