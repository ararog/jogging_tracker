{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views where

import Domain
import GHC.Generics (Generic)
import Web.Scotty
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Control.Monad.IO.Class
import Web.Scotty.Internal.Types

viewUser :: Maybe User -> ActionM ()
viewUser Nothing = json ()
viewUser (Just user) = json user

sessionsList :: [Session] -> ActionM ()
sessionsList sessions = json sessions

viewSession :: Maybe Session -> ActionM ()
viewSession Nothing = json ()
viewSession (Just session) = json session

createdSession :: Maybe Session -> ActionM ()
createdSession session = json ()

updatedSession :: Maybe Session -> ActionM ()
updatedSession session = json ()

deletedSession :: TL.Text -> ActionM ()
deletedSession id = json ()
