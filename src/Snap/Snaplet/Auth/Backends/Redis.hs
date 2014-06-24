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
import           Data.Maybe (fromJust, fromMaybe, isJust)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding as E
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

{--------------------------------------------------------------------------------}
{--- | Type representing the concept of a User in your application.-}
{-data AuthUser = AuthUser-}
    {-{ userId               :: Maybe UserId-}
    {-, userLogin            :: Text-}

    {--- We have to have an email field for password reset functionality, but we-}
    {--- don't want to force users to log in with their email address.-}
    {-, userEmail            :: Maybe Text-}
    {-, userPassword         :: Maybe Password-}
    {-, userActivatedAt      :: Maybe UTCTime-}
    {-, userSuspendedAt      :: Maybe UTCTime-}
    {-, userRememberToken    :: Maybe Text-}
    {-, userLoginCount       :: Int-}
    {-, userFailedLoginCount :: Int-}
    {-, userLockedOutUntil   :: Maybe UTCTime-}
    {-, userCurrentLoginAt   :: Maybe UTCTime-}
    {-, userLastLoginAt      :: Maybe UTCTime-}
    {-, userCurrentLoginIp   :: Maybe ByteString-}
    {-, userLastLoginIp      :: Maybe ByteString-}
    {-, userCreatedAt        :: Maybe UTCTime-}
    {-, userUpdatedAt        :: Maybe UTCTime-}
    {-, userResetToken       :: Maybe Text-}
    {-, userResetRequestedAt :: Maybe UTCTime-}
    {-, userRoles            :: [Role]-}
    {-, userMeta             :: HashMap Text Value-}
    {-}-}
  {-deriving (Show,Eq)-}

userHashKey :: Text -> B.ByteString
{-userHashKey user = B.append (B.fromString "user:") (E.encodeUtf8 user)-}
userHashKey user = B.append "user:" (E.encodeUtf8 user)

enc = E.encodeUtf8

redisSave :: RedisAuthManager -> AuthUser -> IO (Either AuthFailure AuthUser)
redisSave r u = 
    do runRedis (conn r) $ do
         {- AA TODO: Check if user exists and incr next:userid if not -}
         res <- multiExec $ do
           res1 <- hmset (userHashKey $ userLogin u) 
            [("userId",             (enc $ maybe "" unUid $ userId u)),
             ("userLogin",          (enc $ userLogin u)),
             ("userEmail",          (enc $ fromMaybe "" $ userEmail u)),
             ("userPassword",       (enc $ maybe "" (T.pack . show) $ userPassword u)), {- AA TODO: encrypt as necessary -}
             ("userActivatedAt",    (enc $ maybe "" (T.pack . show) $ userActivatedAt u)),
             ("userSuspendedAt",    (enc $ maybe "" (T.pack . show) $ userSuspendedAt u)),
             ("userRememberToken",          (enc $ fromMaybe "" $ userRememberToken u)),
    {-{ userId               :: Maybe UserId-}
    {-, userLogin            :: Text-}
    {--- We have to have an email field for password reset functionality, but we-}
    {--- don't want to force users to log in with their email address.-}
    {-, userEmail            :: Maybe Text-}
    {-, userPassword         :: Maybe Password-}
    {-, userActivatedAt      :: Maybe UTCTime-}
    {-, userSuspendedAt      :: Maybe UTCTime-}
    {-, userRememberToken    :: Maybe Text-}
    {-, userLoginCount       :: Int-}
    {-, userFailedLoginCount :: Int-}
    {-, userLockedOutUntil   :: Maybe UTCTime-}
    {-, userCurrentLoginAt   :: Maybe UTCTime-}
    {-, userLastLoginAt      :: Maybe UTCTime-}
    {-, userCurrentLoginIp   :: Maybe ByteString-}
    {-, userLastLoginIp      :: Maybe ByteString-}
    {-, userCreatedAt        :: Maybe UTCTime-}
    {-, userUpdatedAt        :: Maybe UTCTime-}
    {-, userResetToken       :: Maybe Text-}
    {-, userResetRequestedAt :: Maybe UTCTime-}
    {-, userRoles            :: [Role]-}
    {-, userMeta             :: HashMap Text Value-}
            ("", "")
            ]
           {- AA TODO: set "userid:1000" = "bob" -}
           return $ res1
         case res of
           TxSuccess _ -> return $ Right u
           TxAborted -> return $ Left $ AuthError "redis transaction aborted"
           TxError e -> return $ Left $ AuthError e

{-AA TODO: implement these functions-}
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
