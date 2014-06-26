{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}


module Snap.Snaplet.Auth.Backends.Redis
  ( initRedisAuthManager
  ) where

import           Control.Applicative
import           Control.Monad.State hiding (get)
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

userIdKey :: Text -> B.ByteString
userIdKey userid = enc $ T.append (T.pack $ "userid:") userid

enc :: Text -> B.ByteString
enc = E.encodeUtf8

encMaybeUTCTime :: Maybe UTCTime -> B.ByteString
encMaybeUTCTime (Just u) = enc $ T.pack . show $ u
encMaybeUTCTime _ = ""

encRoles :: [Role] -> B.ByteString
encRoles (r:rs) = enc $ T.pack $ foldl (\x y -> (show x) ++ "," ++ (show y)) (show r) rs

redisSave :: RedisAuthManager -> AuthUser -> IO (Either AuthFailure AuthUser)
redisSave r u = 
    do runRedis (conn r) $ do
         {- AA TODO: Check if user exists and incr next:userid if not -}
         nexti <- nextUserID u
         case nexti of
           Right checkedUserId -> do
             res <- multiExec $ do
               res1 <- hmset (userHashKey $ userLogin u) 
                [("userId",                 (enc $ checkedUserId)),
                 ("userLogin",              (enc $ userLogin u)),
                 ("userEmail",              (enc $ fromMaybe "" $ userEmail u)),
                 ("userPassword",           (enc $ maybe "" (T.pack . show) $ userPassword u)), {- AA TODO: encrypt as necessary -}
                 ("userActivatedAt",        (encMaybeUTCTime $ userActivatedAt u)),
                 ("userSuspendedAt",        (encMaybeUTCTime $ userSuspendedAt u)),
                 ("userRememberToken",      (enc $ fromMaybe "" $ userRememberToken u)),
                 ("userLoginCount",         (enc $ T.pack . show $ userLoginCount u)),
                 ("userFailedLoginCount",   (enc $ T.pack . show $ userFailedLoginCount u)),
                 ("userLockedOutUntil",     (encMaybeUTCTime $ userLockedOutUntil u)),
                 ("userCurrentLoginAt",     (encMaybeUTCTime $ userCurrentLoginAt u)),
                 ("userLastLoginAt",        (encMaybeUTCTime $ userLastLoginAt u)),
                 ("userCurrentLoginIp",     (fromMaybe "" $ userCurrentLoginIp u)),
                 ("userLastLoginIp",        (fromMaybe "" $ userLastLoginIp u)),
                 ("userCreatedAt",          (encMaybeUTCTime $ userCreatedAt u)),
                 ("userUpdatedAt",          (encMaybeUTCTime $ userUpdatedAt u)),
                 ("userResetToken",         (enc $ fromMaybe "" $ userResetToken u)),
                 ("userResetRequestedAt",   (encMaybeUTCTime $ userResetRequestedAt u)),
                 ("userRoles",              (encRoles $ userRoles u)),
                 ("userMeta",               (enc $ T.pack . show $ userMeta u))
                ]
               {- set "userid:1000" = "bob" -}
               set (userIdKey checkedUserId) (enc $ userLogin u)
               return $ res1
             case res of
               TxSuccess _ -> return $ Right u
               TxAborted -> return $ Left $ AuthError "redis transaction aborted"
               TxError e -> return $ Left $ AuthError e
           Left (Error e) -> return $ Left $ AuthError (show e)

redisDestroy :: RedisAuthManager -> AuthUser -> IO ()
redisDestroy r u =
    case (userId u) of
      Nothing -> return ()
      Just uid ->
        do runRedis (conn r) $ do
            del [userHashKey $ userLogin u,
                 (userIdKey $ unUid uid)]
            return ()

redisLookupByUserId :: RedisAuthManager -> UserId -> IO (Maybe AuthUser)
redisLookupByUserId r uid =
  do runRedis (conn r) $ do
      ul <- get (userIdKey $ unUid uid)
      case ul of
        Right (Just userlogin) -> liftIO $ redisLookupByLogin r (T.pack . show $ userlogin)
        _ -> return Nothing

{-AA TODO: implement these functions-}
redisLookupByLogin :: RedisAuthManager -> Text -> IO (Maybe AuthUser)
redisLookupByLogin r ul =
  do runRedis (conn r) $ do
      uhash <- hgetall (userHashKey ul)
      case uhash of
        Right h -> undefined
        Left reply -> undefined

redisLookupByRememberToken :: RedisAuthManager -> Text -> IO (Maybe AuthUser)
redisLookupByRememberToken = undefined

instance IAuthBackend RedisAuthManager where
  save = redisSave
  destroy = redisDestroy
  lookupByUserId = redisLookupByUserId
  lookupByLogin = redisLookupByLogin
  lookupByRememberToken mgr token = error "RedisAuthManager: lookupByRememberToken is not yet implemented"

nextUserID :: AuthUser -> Redis (Either Reply T.Text)
nextUserID u = case (userId u) of
                 Just uid -> return $ Right $ unUid uid
                 Nothing -> do
                   i <- incr "next.userId"
                   case i of
                     Right i -> return $ Right $ T.pack $ show i
                     Left e -> return $ Left e
