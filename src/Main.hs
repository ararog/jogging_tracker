{-# LANGUAGE OverloadedStrings #-}

module Main where

import Db
import Domain
import Utils
import Views

import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Pool(createPool)
import Database.PostgreSQL.Simple
import qualified Data.Text.Lazy as TL
import Data.Time
import qualified Network.HTTP.Types as H
import Network.Wai
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.JWT
import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)

-- Parse file "application.conf" and get the DB connection info
makeDbConfig :: C.Config -> IO (Maybe Db.DbConfig)
makeDbConfig conf = do
  name <- C.lookup conf "database.name" :: IO (Maybe String)
  user <- C.lookup conf "database.user" :: IO (Maybe String)
  pwd <- C.lookup conf "database.password" :: IO (Maybe String)
  return $ DbConfig <$> name
                    <*> user
                    <*> pwd

-- The function knows which resources are available only for the
-- authenticated users
protectedResources ::  Request -> IO Bool
protectedResources req = do
    let path = pathInfo req
    return $ protect path
    where protect (p : _) =  p == "sessions"  -- all requests to /admin/* should be authenticated
          protect _       =  False         -- other requests are allowed for anonymous users


main :: IO ()
main = do
    loadedConf <- C.load [C.Required "application.conf"]
    dbConf <- makeDbConfig loadedConf
    now <- getCurrentTime

    case dbConf of
      Nothing -> putStrLn "No database configuration found, terminating..."
      Just conf -> do
          pool <- createPool (newConn conf) close 1 40 10
          scotty 3000 $ do
              middleware $ staticPolicy (noDots >-> addBase "static") -- serve static files
              middleware $ logStdout                                  -- log all requests; for production use logStdout
              middleware $ jwt "1978@rpa" ["/auth", "/test", "/favicon.ico"]

              post "/auth/login" $ do user <- getUserParam             -- read the request body, try to parse it into user
                                      case user of
                                        Nothing ->
                                           forbidden
                                        Just u -> do
                                           maybeUser <- liftIO $ findUserByLogin pool (email u) (password u)-- get the user from the DB
                                           case maybeUser of
                                             Nothing ->
                                               forbidden
                                             Just mu ->
                                               viewUser $ addToken now mu

              -- LIST
              get    "/sessions" $ do sessions <- liftIO $ listSessions pool  -- get the ist of session for DB
                                      sessionsList sessions                   -- show session list

              -- VIEW
              get    "/sessions/:id" $ do _id <- param "id" :: ActionM TL.Text -- get the article id from the request
                                          maybeSession <- liftIO $ findSession pool _id -- get the session from the DB
                                          viewSession maybeSession           -- show the session if it was found

              -- CREATE
              post   "/sessions" $ do session <- getSessionParam -- read the request body, try to parse it into session
                                      insertSession pool session -- insert the parsed session into the DB
                                      createdSession session     -- show info that the session was created

              -- UPDATE
              put    "/sessions" $ do session <- getSessionParam -- read the request body, try to parse it into session
                                      updateSession pool session -- update parsed session in the DB
                                      updatedSession session     -- show info that the session was updated

              -- DELETE
              delete "/sessions/:id" $ do _id <- param "id" :: ActionM TL.Text -- get the session id
                                          deleteSession pool _id  -- delete the article from the DB
                                          deletedSession _id      -- show info that the session was deleted

-----------------------------------------------

-- Parse the request body into the User
getUserParam :: ActionT TL.Text IO (Maybe User)
getUserParam = do b <- body
                  return (decode b :: Maybe User)

-- Parse the request body into the Session
getSessionParam :: ActionT TL.Text IO (Maybe Session)
getSessionParam = do b <- body
                     return (decode b :: Maybe Session)

forbidden :: ActionM ()
forbidden = do
  status H.forbidden403
  json ()
