{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

{-|

Redis DB snaplet.

-}

module Snap.Snaplet.RedisDB
    (RedisDB
    , runRedisDB
    , redisConnection
    , redisDBInit
    , redisDBInitConf)

where

import Control.Lens
import Control.Monad.State

import Database.Redis hiding (String)
import Data.Configurator as C
import Data.Configurator.Types (Configured(..), Value(..))
import Data.Maybe
import Data.Ratio (numerator, denominator)
import qualified Data.Text as T

import Snap.Snaplet

-- | Snaplet's state data type
newtype RedisDB = RedisDB
                  { _connection :: Connection -- ^ DB connection pool.
                  }

makeLenses ''RedisDB

-- | Instance to allow port to be either a path to a unix socket or a
-- port number.
instance Configured PortID where
  convert (Number r) | denominator r == 1 = Just $ PortNumber $ fromInteger $ numerator r
  convert (String s) = Just $ UnixSocket $ T.unpack s
  convert _ = Nothing

-- | A lens to retrieve the connection to Redis from the 'RedisDB'
-- wrapper.
redisConnection :: Simple Lens RedisDB Connection
redisConnection = connection

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


-- | Make RedisDB snaplet and initialize database connection from
-- snaplet config file. Options are read from the "redis" section of
-- the application config (e.g. ./devel.cfg) or from the main section
-- of the Redis snaplet config (e.g. ./snaplets/redis/devel.cfg).
--
-- Every field is optional and defaults to defaultConnectInfo values.
--
-- > redis {
-- >     host = "192.168.0.42"
-- >     port = 31415
-- >     auth = "i am so secret"
-- >     max_connections = 1
-- >     max_idle_time = 0.5
-- > }
--
-- Alternately, you can configure it to connect via a socket, for example:
--
-- > redis {
-- >     port = "/var/run/redis/redis.sock"
-- > }
--
-- This corresponds to setting:
--
-- > connectPort = UnixSocket "/var/run/redis/redis.sock"
--
-- in `ConnectInfo`. In this case, the host setting, if anything, is
-- ignored.
--
--
-- > appInit :: SnapletInit MyApp MyApp
-- > appInit = makeSnaplet "app" "App with Redis child snaplet" Nothing $
-- >           do
-- >             d <- nestSnaplet "redis" database redisDBInitConf
-- >             return $ MyApp d
redisDBInitConf :: SnapletInit b RedisDB
redisDBInitConf = makeSnaplet "redis" "Redis snaplet." Nothing $ do
    config <- getSnapletUserConfig

    connInfo <- liftIO $ do
        cHost <- C.lookup config "host"
        cPort <- C.lookup config "port"
        cAuth <- C.lookup config "auth"
        cCons <- C.lookup config "max_connections"
        cIdle <- C.lookup config "max_idle_time"

        let def = defaultConnectInfo
        return $ def { connectHost = fromMaybe (connectHost def) cHost
                     , connectPort = fromMaybe (connectPort def) cPort
                     , connectAuth = cAuth
                     , connectMaxConnections =
                       fromMaybe (connectMaxConnections def) cCons
                     , connectMaxIdleTime =
                       maybe (connectMaxIdleTime def) fromRational cIdle
                     }

    conn <- liftIO $ connect connInfo
    return $ RedisDB conn

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
