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
import           Data.Generics
import           Data.HashMap.Strict                 (HashMap)
import qualified Data.HashMap.Strict                 as HM
import           Data.Serialize                      (Serialize)
import qualified Data.Serialize                      as S
import           Data.Text                           (Text)
import           Data.Text.Encoding
import           Data.Monoid
import           Snap.Core                           (Snap)
import           Web.ClientSession
import           Database.Redis
------------------------------------------------------------------------------
import           Snap.Snaplet
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.SessionManager
-------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Session data are kept in a 'HashMap' for this backend
--
type Session = HashMap Text Text


------------------------------------------------------------------------------
-- | This is what the 'Payload' will be for the CookieSession backend
--
-- AA TODO: only the csrftoken should be sent to the client.
--          The Session hash should be stored in Redis
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
    -> ConnectInfo       -- ^ Redis connection info
    -> SnapletInit b SessionManager
initRedisSessionManager fp cn to c =
    makeSnaplet "RedisSession"
                "A snaplet providing sessions via HTTP cookies with a Redis backend."
                Nothing $ liftIO $ do
        key <- getKey fp
        rng <- liftIO mkRNG
        con <- connect c
        return $! SessionManager $ RedisSessionManager Nothing key cn to rng con


------------------------------------------------------------------------------
instance ISessionManager RedisSessionManager where

{-AA TODO: I think only load and commit are supposed to do any IO-}
    --------------------------------------------------------------------------
    load mgr@(RedisSessionManager _ _ _ _ _ con) = do
          pl <- getPayload mgr
          case pl of
            Nothing -> liftIO $ loadDefSession mgr
            Just (Payload x) -> do
              let c = S.decode x
              case c of
                Left _ -> liftIO $ loadDefSession mgr
                Right cs -> liftIO $ do
                  runRedis con $ do
                    l <- hgetall (sessionKey $ rsCSRFToken cs)
                    case l of
                      Left _ -> liftIO $ loadDefSession mgr
                      Right l' -> do
                        let rs = cs { rsSession = HM.fromList $ map decodeTuple l'}
                        return $ mgr { session = Just rs }


    --------------------------------------------------------------------------
    --AA TODO: commit should write to redis and send the csrf to client
    --and also set the timeout properly
    commit mgr@(RedisSessionManager r _ _ _ rng _) = do
        pl <- case r of
                Just r' -> return . Payload $ S.encode r'
                Nothing -> liftIO (mkCookieSession rng) >>=
                           return . Payload . S.encode
        setPayload mgr pl

    --------------------------------------------------------------------------
    reset mgr = do
        cs <- liftIO $ mkCookieSession (randomNumberGenerator mgr)
        return $ mgr { session = Just cs }

    --------------------------------------------------------------------------
    {-AA TODO: Extend the redis timout for the key-}
    touch = id

    --------------------------------------------------------------------------
    insert k v mgr@(RedisSessionManager r _ _ _ _ con) = case r of
        Just r' -> mgr { session = Just $ modSession (HM.insert k v) r' }
        Nothing -> mgr
        {-Just r'@(RedisSession csrf) -> liftIO $-}
          {-runRedis con $ do-}
            {-res1 <- hset (sessionKey csrf)-}
                         {-(encodeUtf8 k)-}
                         {-(encodeUtf8 v)-}
            {-[>return res1<]-}
            {-return mgr-}

    --------------------------------------------------------------------------
    lookup k (RedisSessionManager r _ _ _ _ _) = r >>= HM.lookup k . rsSession
    {-lookup k (RedisSessionManager r _ _ _ _ con) = case r of-}
      {-Nothing -> Nothing-}
      {-Just r'@(RedisSession csrf sess) -> do-}
        {-runRedis con $ do-}
          {-v <- hget (sessionKey csrf) k-}
          {-case v of-}
            {-Nothing -> return Nothing-}
            {-Just v' -> return Just $ decodeUtf8 v'-}

    --------------------------------------------------------------------------
    delete k mgr@(RedisSessionManager r _ _ _ _ _) = case r of
        Just r' -> mgr { session = Just $ modSession (HM.delete k) r' }
        Nothing -> mgr

    {-delete k mgr@(RedisSessionManager r _ _ _ _ con) = case r of-}
        {-Just r'@(RedisSession csrf) -> do-}
          {-runRedis con $ hdel (sessionKey csrf) k-}
          {-mgr-}
        {-Nothing -> mgr-}

    --------------------------------------------------------------------------
    csrf (RedisSessionManager r _ _ _ _ _) = case r of
        Just r' -> rsCSRFToken r'
        Nothing -> ""

    --------------------------------------------------------------------------
    toList (RedisSessionManager r _ _ _ _ _) = case r of
        Just r' -> HM.toList . rsSession $ r'
        Nothing -> []
        {-Just r'@(RedisSession csrf) -> do-}
          {-runRedis con $ do-}
            {-l <- hgetall (sessionKey csrf)-}
            {-case l of-}
              {-Right l' -> return $ map (\(k,v) -> undefined) l'-}
              {-Left _ -> return []-}


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
setPayload mgr x = setSecureCookie (cookieName mgr) (siteKey mgr)
                                   (timeOut mgr) x
