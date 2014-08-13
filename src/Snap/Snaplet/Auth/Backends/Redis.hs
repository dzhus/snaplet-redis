{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

{-|

This module allows you to use the auth snaplet with your user database
stored in a Redis instance.

In your initializer you'll need something like:

@
 a <- nestSnaplet "auth" auth $
          initRedisAuthManager defAuthSettings sess defaultConnectInfo
@


Redis Key Space

The following keys are used to store the user information in Redis.
Be sure to avoid key collisions within your applications.

* next.userId - Int representing the next spare userId.

* user:[username] (eg. user:bob) - Hash of the user fields for user bob.

* userid:[userId] (eg. userid:2 - bob) - Stores username for userId based lookup.

* usertoken:[usertoken] (eg. usertoken:XXXXXXXX - bob) - Remember Token based user lookup.

-}


module Snap.Snaplet.Auth.Backends.Redis
  ( initRedisAuthManager
  ) where

import           Control.Applicative
import           Control.Monad.State hiding (get)
import qualified Data.ByteString as B
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HM
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Text.Encoding as E
import           Data.Time
import           Data.Traversable
import           Database.Redis
import           Snap.Snaplet
import           Text.Read (readMaybe)
import           Web.ClientSession

import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session


------------------------------------------------------------------------------
-- | Initialize a Redis backed 'AuthManager'
initRedisAuthManager :: AuthSettings
                        -- ^ Authentication settings for your app
                        -> SnapletLens b SessionManager
                        -- ^ Lens into a 'SessionManager' auth snaplet will use
                        -> ConnectInfo
                        -- ^ Redis ConnectInfo
                        -> SnapletInit b (AuthManager b)
initRedisAuthManager s l c =
    makeSnaplet
        "RedisAuthManager"
        "A snaplet providing user authentication using a Redis backend"
        Nothing $ liftIO $ do
            rng <- liftIO mkRNG
            key <- getKey (asSiteKey s)
            con <- connect c
            let redisMgr = RedisAuthManager { conn = con }
            return AuthManager {
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

userHashKey :: Text -> B.ByteString
{-userHashKey user = B.append (B.fromString "user:") (E.encodeUtf8 user)-}
userHashKey user = B.append "user:" (E.encodeUtf8 user)

userIdKey :: Text -> B.ByteString
userIdKey userid = enc $ T.append (T.pack "userid:") userid

userTokenKey :: Text -> B.ByteString
userTokenKey usertoken = enc $ T.append (T.pack "usertoken:") usertoken

enc :: Text -> B.ByteString
enc = E.encodeUtf8

dec :: B.ByteString -> Text
dec = E.decodeUtf8

encodeInt :: Int -> B.ByteString
encodeInt = enc . T.pack . show

-- AA TODO: return a Maybe Int here instead.
-- Might be able to use ByteString.Char8.readInt depending on if it's ok
-- with unicode input bytes.
decodeInt :: B.ByteString -> Int
decodeInt = read . T.unpack . dec

encMaybeUTCTime :: Maybe UTCTime -> B.ByteString
encMaybeUTCTime (Just u) = enc $ T.pack . show $ u
encMaybeUTCTime _ = ""

decMaybeUTCTime :: B.ByteString -> Maybe UTCTime
decMaybeUTCTime s = case s of
                            "" -> Nothing
                            _ -> readMaybe . T.unpack . dec $ s

encRoles :: [Role] -> B.ByteString
encRoles [] = ""
encRoles roles = enc $ T.intercalate "," $ map (T.pack . show) roles

decodeRoles :: B.ByteString -> [Role] 
decodeRoles "" = []
decodeRoles s = map (read . T.unpack) $ T.splitOn "," (dec s)

encPassword :: Maybe Password -> B.ByteString
encPassword (Just (Encrypted p)) = p
encPassword (Just (ClearText _)) = error "encPassword should never encode ClearText password"
encPassword Nothing = ""

decPassword :: B.ByteString -> Maybe Password
decPassword "" = Nothing
decPassword p = Just (Encrypted p)

{- Check if user exists and incr next:userid if not -}
nextUserID :: AuthUser -> Redis (Either Reply T.Text)
nextUserID u = case userId u of
                 Just uid -> return $ Right $ unUid uid
                 Nothing -> do
                   i <- incr "next.userId"
                   case i of
                     Right newUserId -> return $ Right $ T.pack $ show newUserId
                     Left e -> return $ Left e

redisSave :: RedisAuthManager -> AuthUser -> IO (Either AuthFailure AuthUser)
redisSave r u = 
    runRedis (conn r) $ do
         nexti <- nextUserID u
         case nexti of
           Right checkedUserId -> do
             res <- multiExec $ do
               res1 <- hmset (userHashKey $ userLogin u) 
                  [("userId",                 enc checkedUserId),
                   ("userLogin",              enc $ userLogin u),
                   ("userEmail",              enc $ fromMaybe "" $ userEmail u),
                   ("userPassword",           encPassword (userPassword u)),
                   ("userActivatedAt",        encMaybeUTCTime $ userActivatedAt u),
                   ("userSuspendedAt",        encMaybeUTCTime $ userSuspendedAt u),
                   ("userRememberToken",      enc $ fromMaybe "" $ userRememberToken u),
                   ("userLoginCount",         encodeInt $ userLoginCount u),
                   ("userFailedLoginCount",   encodeInt $ userFailedLoginCount u),
                   ("userLockedOutUntil",     encMaybeUTCTime $ userLockedOutUntil u),
                   ("userCurrentLoginAt",     encMaybeUTCTime $ userCurrentLoginAt u),
                   ("userLastLoginAt",        encMaybeUTCTime $ userLastLoginAt u),
                   ("userCurrentLoginIp",     fromMaybe "" $ userCurrentLoginIp u),
                   ("userLastLoginIp",        fromMaybe "" $ userLastLoginIp u),
                   ("userCreatedAt",          encMaybeUTCTime $ userCreatedAt u),
                   ("userUpdatedAt",          encMaybeUTCTime $ userUpdatedAt u),
                   ("userResetToken",         enc $ fromMaybe "" $ userResetToken u),
                   ("userResetRequestedAt",   encMaybeUTCTime $ userResetRequestedAt u),
                   ("userRoles",              encRoles $ userRoles u),
                   ("userMeta",               enc $ T.pack . show $ HM.toList $ userMeta u)
                  ]
               {- set "userid:1000" = "bob" -}
               res2 <- set (userIdKey checkedUserId) (enc $ userLogin u)
               {- set "usertoken:XXXX" = "bob" -}
               _ <- traverse (\t -> set (userTokenKey t) (enc $ userLogin u)) $ userRememberToken u
               return $ (,) <$> res1 <*> res2
             case res of
               TxSuccess _ -> return $ Right u
               TxAborted -> return $ Left $ AuthError "redis transaction aborted"
               TxError e -> return $ Left $ AuthError e
           Left (Error e) -> return $ Left $ AuthError (show e)
           Left _ -> return $ Left $ AuthError "redisSave unknown error"

redisDestroy :: RedisAuthManager -> AuthUser -> IO ()
redisDestroy r u =
    case userId u of
      Nothing -> return ()
      Just uid ->
        runRedis (conn r) $ do
            _ <- del [userHashKey $ userLogin u,
                      userIdKey $ unUid uid]
            return ()

redisLookupByUserId :: RedisAuthManager -> UserId -> IO (Maybe AuthUser)
redisLookupByUserId r uid = 
  runRedis (conn r) $ do
      ul <- get (userIdKey $ unUid uid)
      case ul of
        Right (Just userlogin) -> liftIO $ redisLookupByLogin r (dec userlogin)
        _ -> return Nothing

redisLookupByLogin :: RedisAuthManager -> Text -> IO (Maybe AuthUser)
redisLookupByLogin r ul =
  runRedis (conn r) $ do
      uhash <- hgetall (userHashKey ul)
      case uhash of
        Right [] -> return Nothing
        Left _ -> return Nothing
        Right h -> return $ Just $ authUserFromHash h

hmlookup :: B.ByteString -> HashMap B.ByteString B.ByteString -> B.ByteString
hmlookup k hm = fromMaybe "" $ HM.lookup k hm

authUserFromHash :: [(B.ByteString, B.ByteString)] -> AuthUser
authUserFromHash [] = error "authUserFromHash error: Empty hashmap"
authUserFromHash l = 
    let hm = HM.fromList l
         in AuthUser { userId               = Just $ UserId (dec $ hmlookup "userId" hm)
                     , userLogin            = dec $ hmlookup "userLogin" hm
                     , userEmail            = case hmlookup "userEmail " hm of
                                                "" -> Nothing
                                                email -> Just (T.pack . show $ email)
                     , userPassword         = decPassword (hmlookup "userPassword" hm)
                     , userActivatedAt      = decMaybeUTCTime (hmlookup "userActivatedAt" hm)
                     , userSuspendedAt      = decMaybeUTCTime (hmlookup "userSuspendedAt" hm)
                     , userRememberToken    = case hmlookup "userRememberToken" hm of
                                                "" -> Nothing
                                                token -> Just (dec token)
                     , userLoginCount       = decodeInt (hmlookup "userLoginCount" hm)
                     , userFailedLoginCount = decodeInt (hmlookup "userFailedLoginCount" hm)
                     , userLockedOutUntil   = decMaybeUTCTime (hmlookup "userLockedOutUntil" hm)
                     , userCurrentLoginAt   = decMaybeUTCTime (hmlookup "userCurrentLoginAt" hm)
                     , userLastLoginAt      = decMaybeUTCTime (hmlookup "userLastLoginAt" hm)
                     , userCurrentLoginIp   = Just (hmlookup "userCurrentLoginIp" hm)
                     , userLastLoginIp      = Just (hmlookup "userLastLoginIp" hm)
                     , userCreatedAt        = decMaybeUTCTime (hmlookup "userCreatedAt" hm)
                     , userUpdatedAt        = decMaybeUTCTime (hmlookup "userUpdatedAt" hm)
                     , userResetToken       = Just (dec $ hmlookup "userResetToken" hm)
                     , userResetRequestedAt = decMaybeUTCTime (hmlookup "userResetRequestedAt" hm)
                     , userRoles            = decodeRoles (hmlookup "userRoles" hm)
                     {-AA TODO: use toList and fromList for the HashMap serializing.
                     - the snaplet-postgresql-simple project doesnt handle userMeta either.-}
                     , userMeta             = HM.empty
                   }

redisLookupByRememberToken :: RedisAuthManager -> Text -> IO (Maybe AuthUser)
redisLookupByRememberToken r utkn =
  runRedis (conn r) $ do
      ul <- get (userTokenKey utkn)
      case ul of
        Right (Just userlogin) -> liftIO $ redisLookupByLogin r (dec userlogin)
        _ -> return Nothing

data RedisAuthManager = RedisAuthManager {
                      conn :: Connection
                      }

instance IAuthBackend RedisAuthManager where
  save = redisSave
  destroy = redisDestroy
  lookupByUserId = redisLookupByUserId
  lookupByLogin = redisLookupByLogin
  lookupByRememberToken = redisLookupByRememberToken


