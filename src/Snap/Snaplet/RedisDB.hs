{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

{-|

Redis DB snaplet.

-}

module Snap.Snaplet.RedisDB
    (RedisDB
    , runRedisDB
    , redisDBInit)

where

import Control.Lens
import Control.Monad.State

import Database.Redis

import Snap.Snaplet


------------------------------------------------------------------------------
-- | Snaplet's state data type
data RedisDB = RedisDB
    { _connection :: Connection -- ^ DB connection pool.
    }

makeLenses ''RedisDB


------------------------------------------------------------------------------
-- | Perform action using Redis connection from RedisDB snaplet pool
-- (wrapper for 'Database.Redis.runRedis').
--
-- > runRedisDB database $ do
-- >   set "hello" "world"
runRedisDB :: (MonadIO m, MonadState app m) =>
               Simple Lens app (Snaplet RedisDB) -> Redis a -> m a
runRedisDB snaplet action = do
  c <- gets $ view (snaplet . snapletValue . connection)
  liftIO $ runRedis c action


------------------------------------------------------------------------------
-- | Make RedisDB snaplet and initialize database connection.
--
-- > appInit :: SnapletInit MyApp MyApp
-- > appInit = makeSnaplet "app" "App with Redis child snaplet" Nothing $
-- >           do
-- >             d <- nestSnaplet "" database $
-- >                                 redisDBInit defaultConnectInfo
-- >             return $ MyApp d
redisDBInit :: ConnectInfo -- ^ Information for connnecting to a Redis server.
            -> SnapletInit b RedisDB
redisDBInit connInfo =
    makeSnaplet "snaplet-redis" "Redis snaplet." Nothing $ do
      conn <- liftIO $ connect connInfo
      return $ RedisDB conn
