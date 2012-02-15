{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Redis DB snaplet.

-}

module Snap.Snaplet.RedisDB (RedisDB
                            , withRedisDB
                            , redisDBInit)
where

import Prelude hiding ((.)) 
import Control.Category ((.))
import Control.Monad.CatchIO
import Control.Monad.State

import Data.Lens.Common
import Data.Lens.Template
import Data.Text (Text)

import Data.Pool
import Database.Redis.Redis

import Data.Time.Clock

import Snap.Snaplet


------------------------------------------------------------------------------
-- | Description text used in redisDBInit as makeSnaplet argument.
description :: Text
description = "Redis snaplet."

------------------------------------------------------------------------------
-- | Snaplet's state data type
data RedisDB = RedisDB
    { _dbPool :: Pool Redis -- ^ DB connection pool.
    }

makeLens ''RedisDB

------------------------------------------------------------------------------
-- | Perform action using Redis connection from RedisDB snaplet pool.
--
-- 
-- > withRedisDB database $ \db -> do
-- >   r <- liftIO $ hgetall db key
withRedisDB :: (MonadCatchIO m, MonadState app m) => Lens app (Snaplet RedisDB) -> (Redis -> m b) -> m b
withRedisDB snaplet action = do
  p <- gets $ getL (dbPool . snapletValue . snaplet)
  withResource p action


------------------------------------------------------------------------------
-- | Make RedisDB snaplet and initialize database connection. See
-- 'Data.Pool.createPool' for explanation of pool/stripe size values.
--
-- > appInit :: SnapletInit MyApp MyApp
-- > appInit = makeSnaplet "app" "Application with Redis child snaplet" Nothing $
-- >           do
-- >             d <- nestSnaplet "" database $ redisDBInit "127.0.0.1" "6379" 5 5 60
-- >             return $ MyApp d
redisDBInit :: String -- ^ Redis host.
            -> String -- ^ Redis port.
            -> Int -- ^ Connection pool size (stripe count).
            -> Int -- ^ Stripe size (connections per stripe count).
            -> NominalDiffTime -- ^ Keep unused connection open for that long.
            -> SnapletInit b RedisDB
redisDBInit host port poolSize subpoolSize keepAlive = 
    makeSnaplet "snaplet-redis" description Nothing $ do
      pool <- liftIO $ 
              createPool (connect host port) disconnect poolSize keepAlive subpoolSize
      return $ RedisDB pool
