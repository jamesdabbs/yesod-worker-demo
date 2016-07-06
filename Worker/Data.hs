{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Worker.Data where

-- import           Blaze.ByteString.Builder.Char.Utf8  (fromText)
import           Control.Concurrent
import           Control.Monad
import           Data.Monoid                         ((<>))
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Prelude
import           Network.Wai.EventSource
import           Network.Wai.EventSource.EventStream
import           Text.Show                           (show)
import           Yesod
import qualified Yesod.Core.Unsafe                   as Unsafe

import Keenser
import Database.Redis

data Workers = Workers (MVar Manager)

mkYesodSubData "Workers" [parseRoutes|
/count CountR GET
|]

class (Yesod master, RenderMessage master FormMessage) => YesodWorker master where
  workers :: master -> Workers

type WorkerH a = YesodWorker master => HandlerT Workers (HandlerT master IO) a

bootWorkers :: YesodWorker master => HandlerT master IO ()
bootWorkers = do
  foundation <- getYesod
  let (Workers mv) = workers foundation

  conn <- liftIO $ connect defaultConnectInfo
  conf <- mkConf conn $ do
    concurrency 5
    register blah

  forkHandler handleError $ do
    manager <- startProcess conf
    liftIO $ putMVar mv manager

  return ()

handleError :: e -> HandlerT site IO ()
handleError _ = return ()

blah :: Yesod master => Worker (HandlerT master IO) (Text, Int)
blah = Worker "blah" "default" $ \(word, n) ->
  replicateM_ n $ do
    $(logWarn) word
    liftIO . threadDelay $ 1000000

getManager :: YesodWorker master => HandlerT master IO Manager
getManager = do
  foundation <- getYesod
  let (Workers manager) = workers foundation
  liftIO $ readMVar manager

enqueue :: ToJSON a => YesodWorker master => Worker (HandlerT master IO) a -> a -> HandlerT master IO ()
enqueue job args = do
  manager <- getManager
  Keenser.enqueue manager job args

getCountR :: WorkerH Text
getCountR = return "TODO: getCountR"
  -- (Worker mcount _) <- getYesod
  -- n <- liftIO $ readMVar mcount
  -- return . T.pack . show $ n

newWorkers = Workers <$> newEmptyMVar
