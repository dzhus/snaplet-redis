{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}


module Snap.Snaplet.Auth.Backends.Redis
  ( initRedisAuthManager
  ) where

import           Control.Applicative
import           Control.Monad.State hiding (get)
import qualified Data.ByteString as B
import qualified Data.Aeson (Value)
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HM
import           Data.Maybe (fromJust, fromMaybe, isJust)
import           Data.Traversable
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding as E
import           Data.Time
import           Database.Redis
import           Web.ClientSession

import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session

import           Debug.Trace


------------------------------------------------------------------------------
-- | Initialize a Redis backed 'AuthManager'
initRedisAuthManager :: AuthSettings
                            -- ^ Authentication settings for your app
                        -> SnapletLens b SessionManager
                            -- ^ Lens into a 'SessionManager' auth snaplet will
                           -- use
                        -> ConnectInfo
                            -- ^ Redis ConnectInfo
                        -> SnapletInit b (AuthManager b)
initRedisAuthManager s l c = do
    makeSnaplet
        "RedisAuthManager"
        "A snaplet providing user authentication using a Redis backend"
        Nothing $ liftIO $ do
            rng <- liftIO mkRNG
            key <- getKey (asSiteKey s)
            redisMgr <- mkRedisAuthMgr c
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

mkRedisAuthMgr :: ConnectInfo -> IO RedisAuthManager
mkRedisAuthMgr c = do
    conn <- connect c
    return RedisAuthManager { conn = conn }

data RedisAuthManager = RedisAuthManager {
                      conn :: Connection
                      }

userHashKey :: Text -> B.ByteString
{-userHashKey user = B.append (B.fromString "user:") (E.encodeUtf8 user)-}
userHashKey user = B.append "user:" (E.encodeUtf8 user)

userIdKey :: Text -> B.ByteString
userIdKey userid = enc $ T.append (T.pack $ "userid:") userid

userTokenKey :: Text -> B.ByteString
userTokenKey usertoken = enc $ T.append (T.pack $ "usertoken:") usertoken

enc :: Text -> B.ByteString
enc = E.encodeUtf8

encMaybeUTCTime :: Maybe UTCTime -> B.ByteString
encMaybeUTCTime (Just u) = enc $ T.pack . show $ u
encMaybeUTCTime _ = ""

encRoles :: [Role] -> B.ByteString
encRoles (r:rs) = enc $ T.pack $ foldl (\x y -> (show x) ++ "," ++ (show y)) (show r) rs
encRoles [] = ""

decodeRoles :: B.ByteString -> [Role] 
decodeRoles s = map (read . show) $ T.splitOn (T.pack ",") (T.pack . show $ s)

{- Check if user exists and incr next:userid if not -}
nextUserID :: AuthUser -> Redis (Either Reply T.Text)
nextUserID u = case (userId u) of
                 Just uid -> return $ Right $ unUid uid
                 Nothing -> do
                   i <- incr "next.userId"
                   case i of
                     Right i -> return $ Right $ T.pack $ show i
                     Left e -> return $ Left e

redisSave :: RedisAuthManager -> AuthUser -> IO (Either AuthFailure AuthUser)
redisSave r u = 
    do runRedis (conn r) $ do
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
                 ("userMeta",               (enc $ T.pack . show $ HM.toList $ userMeta u))
                ]
               {- set "userid:1000" = "bob" -}
               res2 <- set (userIdKey checkedUserId) (enc $ userLogin u)
               {- set "usertoken:XXXX" = "bob" -}
               traverse (\t -> set (userTokenKey t) (enc $ userLogin u)) $ (userRememberToken u)
               return $ (,) <$> res1 <*> res2
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

redisLookupByLogin :: RedisAuthManager -> Text -> IO (Maybe AuthUser)
redisLookupByLogin r ul =
  do runRedis (conn r) $ do
      uhash <- hgetall (userHashKey ul)
      case uhash of
        Right [] -> return $ Nothing
        Right h -> return $ Just $ authUserFromHash h
        Left _ -> return Nothing

utcTimeFromByteString :: B.ByteString -> Maybe UTCTime
utcTimeFromByteString s = case s of
                            "" -> Nothing
                            _ -> Just $ (read . show $ s)

{-"authUserFromHash h : 
[(\"userRememberToken\",\"\"),
(\"userSuspendedAt\",\"\"),
(\"userCreatedAt\",\"2014-06-28 06:43:15.653473 UTC\"),
(\"userCurrentLoginIp\",\"\"),
(\"userEmail\",\"\"),
(\"userUpdatedAt\",\"2014-06-28 06:43:15.653473 UTC\"),
(\"userLogin\",\"bob\"),
(\"userId\",\"2\"),
(\"userFailedLoginCount\",\"0\"),
(\"userLastLoginIp\",\"\"),
(\"userActivatedAt\",\"\"),
(\"userCurrentLoginAt\",\"\"),
(\"userPassword\",\"Encrypted \\\"sha256|12|E5fK7/QfmwpxGcRG7DdDaw==|5uvnJHbS2DbVjgmPzkn8v/cI9S52Uo1hJ+JDItCdeI4=\\\"\"),
(\"userResetRequestedAt\",\"\"),
(\"userMeta\",\"[]\"),
(\"userRoles\",\"\"),
(\"userLockedOutUntil\",\"\"),
(\"userLoginCount\",\"0\"),
(\"userLastLoginAt\",\"\"),
(\"userResetToken\",\"\")]"
-}
authUserFromHash :: [(B.ByteString, B.ByteString)] -> AuthUser
authUserFromHash  [("userId",                 _userId),
                   ("userLogin",              _userLogin),
                   ("userEmail",              _userEmail),
                   ("userPassword",           _userPassword),
                   ("userActivatedAt",        _userActivatedAt),
                   ("userSuspendedAt",        _userSuspendedAt),
                   ("userRememberToken",      _userRememberToken),
                   ("userLoginCount",         _userLoginCount),
                   ("userFailedLoginCount",   _userFailedLoginCount),
                   ("userLockedOutUntil",     _userLockedOutUntil),
                   ("userCurrentLoginAt",     _userCurrentLoginAt),
                   ("userLastLoginAt",        _userLastLoginAt),
                   ("userCurrentLoginIp",     _userCurrentLoginIp),
                   ("userLastLoginIp",        _userLastLoginIp),
                   ("userCreatedAt",          _userCreatedAt),
                   ("userUpdatedAt",          _userUpdatedAt),
                   ("userResetToken",         _userResetToken),
                   ("userResetRequestedAt",   _userResetRequestedAt),
                   ("userRoles",              _userRoles),
                   ("userMeta",               _userMeta)
                  ] = AuthUser { userId               = Just $ UserId (T.pack . show $ _userId)
                               , userLogin            = (T.pack . show $ _userLogin)
                               , userEmail            = case _userEmail of
                                                          "" -> Nothing
                                                          _ -> Just $ (T.pack . show $ _userEmail)
                               , userPassword         = Just $ (Encrypted _userPassword)
                               , userActivatedAt      = utcTimeFromByteString _userActivatedAt
                               , userSuspendedAt      = utcTimeFromByteString _userSuspendedAt
                               , userRememberToken    = case _userRememberToken of
                                                          "" -> Nothing
                                                          _ -> Just $ (T.pack . show $ _userRememberToken)
                               , userLoginCount       = (read . show $ _userLoginCount)
                               , userFailedLoginCount = (read . show $ _userFailedLoginCount)
                               , userLockedOutUntil   = utcTimeFromByteString _userLockedOutUntil
                               , userCurrentLoginAt   = utcTimeFromByteString _userCurrentLoginAt
                               , userLastLoginAt      = utcTimeFromByteString _userLastLoginAt
                               , userCurrentLoginIp   = Just _userCurrentLoginIp
                               , userLastLoginIp      = Just _userLastLoginIp
                               , userCreatedAt        = utcTimeFromByteString _userCreatedAt
                               , userUpdatedAt        = utcTimeFromByteString _userUpdatedAt
                               , userResetToken       = Just $ (T.pack . show $ _userResetToken)
                               , userResetRequestedAt = utcTimeFromByteString _userResetRequestedAt
                               , userRoles            = decodeRoles _userRoles
                               {-AA TODO: use toList and fromList for the HashMap serializing.
                               - the snaplet-postgresql-simple project
                               - doesnt handle userMeta either.-}
                               , userMeta             = HM.empty
                             }
authUserFromHash h = traceShow ("authUserFromHash h : " ++ (show h))
                               error "authUserFromHash unknown pattern matching"

redisLookupByRememberToken :: RedisAuthManager -> Text -> IO (Maybe AuthUser)
redisLookupByRememberToken r utkn =
  do runRedis (conn r) $ do
      ul <- get (userTokenKey utkn)
      case ul of
        Right (Just userlogin) -> liftIO $ redisLookupByLogin r (T.pack . show $ userlogin)
        _ -> return Nothing

instance IAuthBackend RedisAuthManager where
  save = redisSave
  destroy = redisDestroy
  lookupByUserId = redisLookupByUserId
  lookupByLogin = redisLookupByLogin
  lookupByRememberToken = redisLookupByRememberToken 


