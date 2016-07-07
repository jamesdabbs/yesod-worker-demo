 module Jobs
  ( allJobs
  , blah
  ) where

import Import
import Yesod.Worker

allJobs = do
  register blah

blah :: Worker Handler (Text, Int)
blah = Worker "blah" "default" $ \(word, n) ->
  replicateM_ n $ do
    $(logWarn) word
    liftIO . threadDelay $ 1000000
