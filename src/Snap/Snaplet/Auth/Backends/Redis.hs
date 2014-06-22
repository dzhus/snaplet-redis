{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}


module Snap.Snaplet.Auth.Backends.Redis
  ( initRedisAuthManager
  ) where

import           Control.Applicative
import           Control.Monad.State
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.Map as HM
import           Data.Map (Map)
import           Data.Maybe (fromJust, isJust)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Database.Redis
import           Web.ClientSession

import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session



------------------------------------------------------------------------------
-- | Initialize a Redis backed 'AuthManager'
initRedisAuthManager :: AuthSettings
                            -- ^ Authentication settings for your app
                        -> SnapletLens b SessionManager
                            -- ^ Lens into a 'SessionManager' auth snaplet will
                           -- use
                        -> Connection
                            -- ^ Redis Connection
                        -> SnapletInit b (AuthManager b)
initRedisAuthManager s l conn = do
    makeSnaplet
        "RedisAuthManager"
        "A snaplet providing user authentication using a Redis backend"
        Nothing $ liftIO $ do
            rng <- liftIO mkRNG
            key <- getKey (asSiteKey s)
            redisMgr <- mkRedisAuthMgr conn
            return $! AuthManager {
                         backend               = redisMgr
                       , session               = l
                       , activeUser            = Nothing
                       , minPasswdLen          = asMinPasswdLen s
                       , rememberCookieName    = asRememberCookieName s
                       , rememberPeriod        = asRememberPeriod s
                       , siteKey               = key
                       , lockout               = asLockout s
                       , randomNumberGenerator = rng
                       }

{-mkRedisAuthMgr :: Connection -> IO RedisAuthManager-}
mkRedisAuthMgr conn = return RedisAuthManager { conn = conn }

data RedisAuthManager = RedisAuthManager {
                      conn :: Connection
                      }

{-AA TODO: implement these functions-}
redisSave :: RedisAuthManager -> AuthUser -> IO (Either AuthFailure AuthUser)
redisSave r u = undefined
redisDestroy :: RedisAuthManager -> AuthUser -> IO ()
redisDestroy = undefined
redisLookupByUserId :: RedisAuthManager -> UserId -> IO (Maybe AuthUser)
redisLookupByUserId = undefined
redisLookupByLogin :: RedisAuthManager -> Text -> IO (Maybe AuthUser)
redisLookupByLogin = undefined
redisLookupByRememberToken :: RedisAuthManager -> Text -> IO (Maybe AuthUser)
redisLookupByRememberToken = undefined

instance IAuthBackend RedisAuthManager where
  save = error "RedisAuthManager: save is not yet implemented"
  destroy = error "RedisAuthManager: destroy is not yet implemented"
  lookupByUserId mgr uid = error "RedisAuthManager: lookupByUserId is not yet implemented"
  lookupByLogin mgr login = error "RedisAuthManager: lookupByLogin is not yet implemented"
  lookupByRememberToken mgr token = error "RedisAuthManager: lookupByRememberToken is not yet implemented"
