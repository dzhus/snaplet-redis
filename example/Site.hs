{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

------------------------------------------------------------------------------
import           Control.Monad.Trans
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString (ByteString)
import           Control.Lens
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.Redis as R
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.Redis
import           Snap.Snaplet.RedisDB
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.RedisSession


------------------------------------------------------------------------------
data App = App
    { _sess :: Snaplet SessionManager
    , _db   :: Snaplet RedisDB
    , _auth :: Snaplet (AuthManager App)
    }

makeLenses ''App

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/",           writeText "hello")
         , ("foo",         fooHandler)
         , ("add/:uname",  addHandler)
         , ("find/:email", findHandler)
         ]

fooHandler :: Handler App App ()
fooHandler = do
    env <- with auth get
    results <- runRedisDB db $ do
        r <- R.keys "user:*"
        case r of
            Left  _       -> error "redis error"
            Right ulogins -> forM ulogins $ \l ->
                liftIO (lookupByLogin env (T.drop 5 $ T.decodeUtf8 l))
    liftIO $ print (results :: [Maybe AuthUser])

addHandler :: Handler App App ()
addHandler = do
    mname <- getParam "uname"
    email <- getParam "email"
    let name = maybe "guest" T.decodeUtf8 mname
    u <- with auth $
        createUser name "" >>= \u -> case u of
            Left _   -> return u
            Right u' -> saveUser (u' {userEmail = T.decodeUtf8 <$> email})
    liftIO $ print u

#if MIN_VERSION_snap(1,1,0)
findHandler :: Handler App App ()
findHandler = do
    email <- getParam "email"
    env <- with auth get
    liftIO $ lookupByEmail env (maybe "" T.decodeUtf8 email) >>= print
#else
findHandler = return ()
#endif

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    d <- nestSnaplet "db" db redisDBInitConf
    s <- nestSnaplet "" sess $
         initRedisSessionManager "site_key.txt"
                                 "_cookie" Nothing (Just $ 60 * 60) (d ^. snapletValue)
    a <- nestSnaplet "auth" auth $ initRedisAuthManager sess (d ^. snapletValue)
    addRoutes routes
    return $ App s d a


main :: IO ()
main = serveSnaplet defaultConfig app
