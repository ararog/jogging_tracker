{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db where
import Domain

import Web.Scotty.Internal.Types (ActionT)
import GHC.Generics (Generic)
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple
import Data.Pool(Pool, createPool, withResource)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import GHC.Int

-- DbConfig contains info needed to connect to MySQL server
data DbConfig = DbConfig {
     dbName :: String,
     dbUser :: String,
     dbPassword :: String
     }
     deriving (Show, Generic)

-- The function knows how to create new DB connection
-- It is needed to use with resource pool
newConn :: DbConfig -> IO Connection
newConn conf = connect defaultConnectInfo
                       { connectUser = dbUser conf
                       , connectPassword = dbPassword conf
                       , connectDatabase = dbName conf
                       }

--------------------------------------------------------------------------------
-- Utilities for interacting with the DB.
-- No transactions.
--
-- Accepts arguments
fetch :: (FromRow r, ToRow q) => Pool Connection -> q -> Query -> IO [r]
fetch pool args sql = withResource pool retrieve
      where retrieve conn = query conn sql args

-- No arguments -- just pure sql
fetchSimple :: FromRow r => Pool Connection -> Query -> IO [r]
fetchSimple pool sql = withResource pool retrieve
       where retrieve conn = query_ conn sql

-- Update database
execSql :: ToRow q => Pool Connection -> q -> Query -> IO Int64
execSql pool args sql = withResource pool ins
       where ins conn = execute conn sql args

-------------------------------------------------------------------------------
-- Utilities for interacting with the DB.
-- Transactions.
--
-- Accepts arguments
fetchT :: (FromRow r, ToRow q) => Pool Connection -> q -> Query -> IO [r]
fetchT pool args sql = withResource pool retrieve
      where retrieve conn = withTransaction conn $ query conn sql args

-- No arguments -- just pure sql
fetchSimpleT :: FromRow r => Pool Connection -> Query -> IO [r]
fetchSimpleT pool sql = withResource pool retrieve
       where retrieve conn = withTransaction conn $ query_ conn sql

-- Update database
execSqlT :: ToRow q => Pool Connection -> q -> Query -> IO Int64
execSqlT pool args sql = withResource pool ins
       where ins conn = withTransaction conn $ execute conn sql args

--------------------------------------------------------------------------------

findUserByLogin :: Pool Connection -> TL.Text -> TL.Text -> IO (Maybe User)
findUserByLogin pool email password = do
         res <- liftIO $ fetch pool [email, password] "SELECT * FROM users WHERE email=? AND password=?" :: IO [(Integer, TL.Text, TL.Text)]
         return $ oneUser res
         where oneUser ((id, email, password) : _) = Just $ User id email password ""
               oneUser _ = Nothing

findUser :: Pool Connection -> TL.Text -> IO (Maybe User)
findUser pool id = do
    res <- fetch pool (Only id) "SELECT * FROM users WHERE id=?" :: IO [(Integer, TL.Text, TL.Text)]
    return $ oneUser res
    where oneUser ((id, email, password) : _) = Just $ User id email password ""
          oneUser _ = Nothing

--------------------------------------------------------------------------------

listSessions :: Pool Connection -> IO [Session]
listSessions pool = do
     res <- fetchSimple pool "SELECT * FROM sessions ORDER BY id DESC" :: IO [(Integer, TL.Text, Float, TL.Text, TL.Text, TL.Text, TL.Text, Float)]
     return $ map (\(id, date, distance, time, gps_path, weather, photo, avg_speed) -> Session id date distance time gps_path weather photo avg_speed) res

findSession :: Pool Connection -> TL.Text -> IO (Maybe Session)
findSession pool id = do
     res <- fetch pool (Only id) "SELECT * FROM sessions WHERE id=?" :: IO [(Integer, TL.Text, Float, TL.Text, TL.Text, TL.Text, TL.Text, Float)]
     return $ oneSession res
     where oneSession ((id, date, distance, time, gps_path, weather, photo, avg_speed) : _) = Just $ Session id date distance time gps_path weather photo avg_speed
           oneSession _ = Nothing


insertSession :: Pool Connection -> Maybe Session -> ActionT TL.Text IO ()
insertSession pool Nothing = return ()
insertSession pool (Just (Session id date distance time gps_path weather photo avg_speed)) = do
     liftIO $ execSqlT pool [date, date, time, gps_path, weather, photo, date]
                            "INSERT INTO sessions(date, distance, time, gps_path, weather, photo, avg_speed) VALUES(?,?,?,?,?,?,?)"
     return ()

updateSession :: Pool Connection -> Maybe Session -> ActionT TL.Text IO ()
updateSession pool Nothing = return ()
updateSession pool (Just (Session id date distance time gps_path weather photo avg_speed)) = do
     liftIO $ execSqlT pool [date, date, time, gps_path, weather, photo, date, (TL.decodeUtf8 $ BL.pack $ show id)]
                            "UPDATE sessions SET date=?, distance=?, time=?, gps_path=?, weather=?, photo=?, avg_speed=? WHERE id=?"
     return ()

deleteSession :: Pool Connection -> TL.Text -> ActionT TL.Text IO ()
deleteSession pool id = do
     liftIO $ execSqlT pool [id] "DELETE FROM sessions WHERE id=?"
     return ()
