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
{-import qualified Data.Aeson (Value)-}
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HM
import           Data.Maybe (fromMaybe)
import           Data.Traversable
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding as E
import           Data.Time
import           Database.Redis
import           Web.ClientSession
import           Text.Read (readMaybe)

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
    con <- connect c
    return RedisAuthManager { conn = con }

data RedisAuthManager = RedisAuthManager {
                      conn :: Connection
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

decodeInt :: B.ByteString -> Int
decodeInt = read . T.unpack . dec

encMaybeUTCTime :: Maybe UTCTime -> B.ByteString
encMaybeUTCTime (Just u) = enc $ T.pack . show $ u
encMaybeUTCTime _ = ""

decMaybeUTCTime :: B.ByteString -> Maybe UTCTime
decMaybeUTCTime s = case s of
                            "" -> Nothing
                            {-_ -> Just $ traceShow (B.append "decMaybeUTCTime s : " s) -}
                                                  {-(read . T.unpack . dec $ s)-}
                            _ -> traceShow (B.append "decMaybeUTCTime s : " s) 
                                           (readMaybe . T.unpack . dec $ s)

encRoles :: [Role] -> B.ByteString
encRoles (r:rs) = enc $ T.pack $ foldl (\x y -> show x ++ "," ++ show y) (show r) rs
encRoles [] = ""

decodeRoles :: B.ByteString -> [Role] 
decodeRoles "" = []
decodeRoles s = traceShow (B.append "decodeRoles s : " s) 
                          map (read . T.unpack) $ T.splitOn (T.pack ",") (dec s)

encPassword :: Maybe Password -> B.ByteString
encPassword (Just (Encrypted p)) = traceShow ("encPassword p: " ++ show p) p
encPassword (Just (ClearText _)) = error "encPassword should never encode ClearText password"
encPassword Nothing = ""

decPassword :: B.ByteString -> Maybe Password
decPassword "" = Nothing
decPassword p = Just $ traceShow ("decPassword - " ++ show p) (Encrypted p)

{- Check if user exists and incr next:userid if not -}
nextUserID :: AuthUser -> Redis (Either Reply T.Text)
nextUserID u = case (userId u) of
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
                  [("userId",                 enc $ checkedUserId),
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
               _ <- traverse (\t -> set (userTokenKey t) (enc $ userLogin u)) $ (userRememberToken u)
               return $ (,) <$> res1 <*> res2
             case res of
               TxSuccess _ -> return $ Right u
               TxAborted -> return $ Left $ AuthError "redis transaction aborted"
               TxError e -> return $ Left $ AuthError e
           Left (Error e) -> return $ Left $ AuthError (show e)
           Left _ -> return $ Left $ AuthError "redisSave unknown error"

redisDestroy :: RedisAuthManager -> AuthUser -> IO ()
redisDestroy r u =
    case (userId u) of
      Nothing -> return ()
      Just uid ->
        runRedis (conn r) $ do
            _ <- del [userHashKey $ userLogin u,
                     (userIdKey $ unUid uid)]
            return ()

redisLookupByUserId :: RedisAuthManager -> UserId -> IO (Maybe AuthUser)
redisLookupByUserId r uid = 
  runRedis (conn r) $ do
      ul <- traceShow (B.append "redisLookupByUserId  uid: " (enc . unUid $ uid)) 
                      get (userIdKey $ unUid uid)
      case ul of
        Right (Just userlogin) -> liftIO $ redisLookupByLogin r (T.pack . show $ userlogin)
        _ -> return Nothing

redisLookupByLogin :: RedisAuthManager -> Text -> IO (Maybe AuthUser)
redisLookupByLogin r ul =
  runRedis (conn r) $ do
      uhash <- traceShow (B.append "redisLookupByLogin  ul: " (enc ul)) 
                         hgetall (userHashKey ul)
      case uhash of
        Right [] -> traceShow "redisLookupByLogin - Right []" $ return Nothing
        Left _ -> traceShow "redisLookupByLogin - Left _" $ return Nothing
        Right h -> traceShow "redisLookupByLogin - Right h" $ return $ Just $ authUserFromHash h

hmlookup :: B.ByteString -> (HashMap B.ByteString B.ByteString) -> B.ByteString
hmlookup k hm = case (HM.lookup k hm) of
                Just s -> traceShow ("hmlookup k: " ++ (show k) ++ " = " ++ (show s))
                                    s
                Nothing -> ""

authUserFromHash :: [(B.ByteString, B.ByteString)] -> AuthUser
authUserFromHash [] = error "authUserFromHash error: Empty hashmap"
authUserFromHash l = 
    let hm = traceShow ("authUserFromHash l:" ++ (show l)) HM.fromList l
         in AuthUser { userId               = Just $ UserId (T.pack . show $ hmlookup "userId" hm)
                     , userLogin            = (T.pack . show $ hmlookup "userLogin" hm)
                     , userEmail            = case hmlookup "userEmail " hm of
                                                "" -> Nothing
                                                email -> Just (T.pack . show $ email)
                     , userPassword         = decPassword (hmlookup "userPassword" hm)
                     , userActivatedAt      = decMaybeUTCTime (hmlookup "userActivatedAt" hm)
                     , userSuspendedAt      = decMaybeUTCTime (hmlookup "userSuspendedAt" hm)
                     , userRememberToken    = case hmlookup "userRememberToken" hm of
                                                "" -> Nothing
                                                token -> Just (T.pack . show $ token)
                     , userLoginCount       = traceShow ("authUserFromHash userLoginCount:" ++ (show $ hmlookup "userLoginCount" hm)) 
                                                        (decodeInt (hmlookup "userLoginCount" hm))
                     , userFailedLoginCount = traceShow ("authUserFromHash userFailedLoginCount:" ++ (show $ hmlookup "userFailedLoginCount" hm)) 
                                                        (decodeInt (hmlookup "userFailedLoginCount" hm))
                     , userLockedOutUntil   = decMaybeUTCTime (hmlookup "userLockedOutUntil" hm)
                     , userCurrentLoginAt   = decMaybeUTCTime (hmlookup "userCurrentLoginAt" hm)
                     , userLastLoginAt      = decMaybeUTCTime (hmlookup "userLastLoginAt" hm)
                     , userCurrentLoginIp   = Just (hmlookup "userCurrentLoginIp" hm)
                     , userLastLoginIp      = Just (hmlookup "userLastLoginIp" hm)
                     , userCreatedAt        = decMaybeUTCTime (hmlookup "userCreatedAt" hm)
                     , userUpdatedAt        = decMaybeUTCTime (hmlookup "userUpdatedAt" hm)
                     , userResetToken       = Just (T.pack . show $ hmlookup "userResetToken" hm)
                     , userResetRequestedAt = decMaybeUTCTime (hmlookup "userResetRequestedAt" hm)
                     , userRoles            = decodeRoles (hmlookup "userRoles" hm)
                     {-AA TODO: use toList and fromList for the HashMap serializing.
                     - the snaplet-postgresql-simple project
                     - doesnt handle userMeta either.-}
                     , userMeta             = HM.empty
                   }

redisLookupByRememberToken :: RedisAuthManager -> Text -> IO (Maybe AuthUser)
redisLookupByRememberToken r utkn =
  runRedis (conn r) $ do
      ul <- traceShow (B.append "redisLookupByRememberToken  utkn: " (enc utkn)) 
                      get (userTokenKey utkn)
      case ul of
        Right (Just userlogin) -> liftIO $ redisLookupByLogin r (T.pack . show $ userlogin)
        _ -> return Nothing

instance IAuthBackend RedisAuthManager where
  save = redisSave
  destroy = redisDestroy
  lookupByUserId = redisLookupByUserId
  lookupByLogin = redisLookupByLogin
  lookupByRememberToken = redisLookupByRememberToken


