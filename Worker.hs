{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Worker where

import Prelude (IO)
import Worker.Data
import Yesod

instance YesodWorker master => YesodSubDispatch Worker (HandlerT master IO) where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesWorker)
