------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Snap.Snaplet.Session.Backends.RedisSession
    ( initRedisSessionManager
    ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Reader
import           Data.ByteString                     (ByteString)
import           Data.HashMap.Strict                 (HashMap)
import qualified Data.HashMap.Strict                 as HM
import           Data.Monoid
import           Data.Serialize                      (Serialize)
import qualified Data.Serialize                      as S
import           Data.Text                           (Text)
import           Data.Text.Encoding
import           Data.Typeable
-- import           GHC.Generics
import           Snap.Core                           (Snap)
import           Web.ClientSession
import           Database.Redis
------------------------------------------------------------------------------
import           Snap.Snaplet
import           Snap.Snaplet.RedisDB
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.SessionManager
-------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Session data are kept in a 'HashMap' for this backend
--
type Session = HashMap Text Text


------------------------------------------------------------------------------
-- | This is what the 'Payload' will be for the RedisSession backend 
-- | Only the rsCSRFToken is sent to the client.
-- | The Session hash is stored in Redis.
data RedisSession = RedisSession
    { rsCSRFToken :: Text
    , rsSession :: Session
    }
  deriving (Eq, Show)


------------------------------------------------------------------------------
--Only serialize the rsCSRFToken to send to the client
instance Serialize RedisSession where
    put (RedisSession a _) =
        S.put $ encodeUtf8 a
    get                     =
        let unpack a = RedisSession (decodeUtf8 a) HM.empty
        in  unpack <$> S.get


encodeTuple :: (Text, Text) -> (ByteString, ByteString)
encodeTuple (a,b) = (encodeUtf8 a, encodeUtf8 b)


decodeTuple :: (ByteString, ByteString) -> (Text, Text)
decodeTuple (a,b) = (decodeUtf8 a, decodeUtf8 b)


------------------------------------------------------------------------------
mkCookieSession :: RNG -> IO RedisSession
mkCookieSession rng = do
    t <- liftIO $ mkCSRFToken rng
    return $ RedisSession t HM.empty


------------------------------------------------------------------------------
-- | The manager data type to be stuffed into 'SessionManager'
--
data RedisSessionManager = RedisSessionManager {
      session               :: Maybe RedisSession
        -- ^ Per request cache for 'CookieSession'
    , siteKey               :: Key
        -- ^ A long encryption key used for secure cookie transport
    , cookieName            :: ByteString
        -- ^ Cookie name for the session system
    , timeOut               :: Maybe Int
        -- ^ Session cookies will be considered "stale" after this many
        -- seconds.
    , randomNumberGenerator :: RNG
        -- ^ handle to a random number generator
    , redisConnection :: Connection
        -- ^ Redis connection to store session info
} deriving (Typeable)


------------------------------------------------------------------------------
loadDefSession :: RedisSessionManager -> IO RedisSessionManager
loadDefSession mgr@(RedisSessionManager ses _ _ _ rng _) =
    case ses of
      Nothing -> do ses' <- mkCookieSession rng
                    return $! mgr { session = Just ses' }
      Just _  -> return mgr


------------------------------------------------------------------------------
modSession :: (Session -> Session) -> RedisSession -> RedisSession
modSession f (RedisSession t ses) = RedisSession t (f ses)

------------------------------------------------------------------------------
sessionKey :: Text -> ByteString
sessionKey t = encodeUtf8 $ mappend "session:" t

------------------------------------------------------------------------------
-- | Initialize a cookie-backed session, returning a 'SessionManager' to be
-- stuffed inside your application's state. This 'SessionManager' will enable
-- the use of all session storage functionality defined in
-- 'Snap.Snaplet.Session'
--
initRedisSessionManager
    :: FilePath             -- ^ Path to site-wide encryption key
    -> ByteString           -- ^ Session cookie name
    -> Maybe Int            -- ^ Session time-out (replay attack protection)
    -> RedisDB              -- ^ Redis connection
    -> SnapletInit b SessionManager
initRedisSessionManager fp cn to c =
    makeSnaplet "RedisSession"
                "A snaplet providing sessions via HTTP cookies with a Redis backend."
                Nothing $ liftIO $ do
        key <- getKey fp
        rng <- liftIO mkRNG
        return $! SessionManager
               $  RedisSessionManager Nothing key cn to rng (_connection c)


------------------------------------------------------------------------------
instance ISessionManager RedisSessionManager where

    --------------------------------------------------------------------------
    --load grabs the session from redis.
    load mgr@(RedisSessionManager r _ _ _ rng con) = do
      case r of
        Just _  -> return mgr
        Nothing -> do
          pl <- getPayload mgr
          case pl of
            Nothing          -> liftIO $ loadDefSession mgr
            Just (Payload x) -> do
              let c = S.decode x
              case c of
                Left _   -> liftIO $ loadDefSession mgr
                Right cs -> liftIO $ do
                  sess <- runRedis con $ do
                    l <- hgetall (sessionKey $ rsCSRFToken cs)
                    case l of
                      Left _   -> liftIO $ mkCookieSession rng
                      Right l' -> do
                        let rs = cs { rsSession = HM.fromList $ map decodeTuple l'}
                        return $ rs
                  return mgr { session = Just sess }

    --------------------------------------------------------------------------
    --commit writes to redis and sends the csrf to client and also sets the 
    --timeout.
    commit mgr@(RedisSessionManager r _ _ to rng con) = do
        pl <- case r of
          Just r' -> liftIO $ do
            runRedis con $ do
              res <- multiExec $ do
                _ <- del [(sessionKey (rsCSRFToken r'))]   --Clear old values
                let sess = map encodeTuple $ HM.toList (rsSession r')
                res1 <- case sess of
                  [] -> hmset (sessionKey (rsCSRFToken r')) [("","")]
                  _  -> hmset (sessionKey (rsCSRFToken r')) sess
                res2 <- case to of
                  Just i  -> expire (sessionKey (rsCSRFToken r')) $ toInteger i
                  Nothing -> persist (sessionKey (rsCSRFToken r'))
                return $ (,) <$> res1 <*> res2
              case res of
                TxSuccess _ -> return . Payload $ S.encode r'
                TxError e   -> error e
                TxAborted   -> error "transaction aborted"
          Nothing -> liftIO (mkCookieSession rng) >>=
                     return . Payload . S.encode
        setPayload mgr pl


    --------------------------------------------------------------------------
    --clear the session from redis and return a new empty one
    {-reset mgr@(RedisSessionManager _ _ _ _ _ _)  = trace "RedisSessionManager reset" $ do-}
    reset mgr@(RedisSessionManager r _ _ _ _ con)  = do
        case r of
          Just r' -> liftIO $ do
            runRedis con $ do
              res1 <- del [(sessionKey $ rsCSRFToken r')]
              case res1 of
                Left e  -> error $ show e
                _ -> return ()
          _ -> return ()
        cs <- liftIO $ mkCookieSession (randomNumberGenerator mgr)
        return $ mgr { session = Just cs }

    --------------------------------------------------------------------------
    touch = id

    --------------------------------------------------------------------------
    insert k v mgr@(RedisSessionManager r _ _ _ _ _) = case r of
        Just r' -> mgr { session = Just $ modSession (HM.insert k v) r' }
        Nothing -> mgr

    --------------------------------------------------------------------------
    lookup k (RedisSessionManager r _ _ _ _ _) = r >>= HM.lookup k . rsSession

    --------------------------------------------------------------------------
    delete k mgr@(RedisSessionManager r _ _ _ _ _) = case r of
        Just r' -> mgr { session = Just $ modSession (HM.delete k) r' }
        Nothing -> mgr

    --------------------------------------------------------------------------
    csrf (RedisSessionManager r _ _ _ _ _) = case r of
        Just r' -> rsCSRFToken r'
        Nothing -> ""

    --------------------------------------------------------------------------
    toList (RedisSessionManager r _ _ _ _ _) = case r of
        Just r' -> HM.toList . rsSession $ r'
        Nothing -> []

------------------------------------------------------------------------------
-- | A session payload to be stored in a SecureCookie.
newtype Payload = Payload ByteString
  deriving (Eq, Show, Ord, Serialize)


------------------------------------------------------------------------------
-- | Get the current client-side value
getPayload :: RedisSessionManager -> Snap (Maybe Payload)
getPayload mgr = getSecureCookie (cookieName mgr) (siteKey mgr) (timeOut mgr)


------------------------------------------------------------------------------
-- | Set the client-side value
setPayload :: RedisSessionManager -> Payload -> Snap ()
setPayload mgr x = setSecureCookie (cookieName mgr) Nothing (siteKey mgr)
                                   (timeOut mgr) x
