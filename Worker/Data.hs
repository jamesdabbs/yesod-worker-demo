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

data Worker = Worker (MVar Int) (MVar ())

mkYesodSubData "Worker" [parseRoutes|
/count CountR GET POST
|]

class (Yesod master, RenderMessage master FormMessage) => YesodWorker master where
  workers :: master -> Worker

type WorkerH a = YesodWorker master => HandlerT Worker (HandlerT master IO) a

bootWorkers :: YesodWorker master => HandlerT master IO ()
bootWorkers = do
  foundation <- getYesod
  let (Worker mcount _) = workers foundation
  forkHandler handleError (forever $ tick mcount)
  return ()

handleError :: e -> HandlerT site IO ()
handleError _ = return ()

tick :: Yesod master => MVar Int -> HandlerT master IO ()
tick mv = do
  n <- liftIO $ takeMVar mv
  $(logWarn) $ "count is now " <> T.pack (show n)
  liftIO $ do
    putMVar mv (n + 7)
    threadDelay 3000000

resetWorkers :: YesodWorker master => HandlerT master IO ()
resetWorkers = do
  (Worker mcount _) <- workers <$> getYesod
  liftIO $ modifyMVar_ mcount $ \n -> return (n + 1)
  return ()

getCountR :: WorkerH Text
getCountR = do
  (Worker mcount _) <- getYesod
  n <- liftIO $ readMVar mcount
  return . T.pack . show $ n

postCountR :: WorkerH ()
postCountR = return ()

newWorker = Worker <$> newMVar 0 <*> newMVar ()
