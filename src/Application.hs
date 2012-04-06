{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, FlexibleInstances, TypeFamilies #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
--   handler monad.
--
module Application where

------------------------------------------------------------------------------
import Control.Monad.State
import Data.Data
import Data.Lens.Template
import Data.Time.Clock
import Data.Text (Text)
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Text.Templating.Heist
import Text.XmlHtml hiding (render)
import Web.Routes
import Web.Routes.TH

------------------------------------------------------------------------------

data AppURL
    = Count Int
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePathInfo ''AppURL)

data App = App
    { _heist     :: Snaplet (Heist App)
    , _startTime :: UTCTime
    , _routeFn   :: AppURL -> [(Text, Maybe Text)] -> Text
    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist


------------------------------------------------------------------------------
type AppHandler = Handler App App


------------------------------------------------------------------------------
-- extra instances and functions to support web-routes

instance MonadRoute (Handler App App) where
    type URL (Handler App App) = AppURL
    askRouteFn = gets _routeFn

instance (MonadRoute m) => MonadRoute (HeistT m) where
    type URL (HeistT m) = URL m
    askRouteFn = lift askRouteFn

heistURL :: MonadRoute m => URL m -> m [Node]
heistURL u =
    do t <- showURL u
       return [TextNode t]
