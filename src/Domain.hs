{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Domain where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative

data User = User {
  id :: Integer,
  email :: Text,
  password :: Text,
  access_token :: Text
  } deriving (Show)

instance FromJSON User where
     parseJSON (Object v) = User <$>
                            v .:? "id" .!= 0 <*> -- the field "id" is optional
                            v .:  "email"    <*>
                            v .:  "password" <*>
                            v .:? "access_token" .!= ""

instance ToJSON User where
     toJSON (User id email _ access_token) =
         object ["id" .= id,
                 "email" .= email,
                 "access_token" .= access_token]

data Session = Session {
  id :: Integer,
  date :: Text,
  distance :: Float,
  time :: Text,
  gps_path :: Text,
  weather :: Text,
  photo :: Text,
  avg_speed :: Float
  } deriving (Show)

instance FromJSON Session where
     parseJSON (Object v) = Session <$>
                            v .:? "id" .!= 0 <*> -- the field "id" is optional
                            v .:  "date"     <*>
                            v .:  "distance" <*>
                            v .:  "time"     <*>
                            v .:  "gps_path" <*>
                            v .:  "weather"  <*>
                            v .:  "photo"    <*>
                            v .:  "avg_speed"

instance ToJSON Session where
     toJSON (Session id date distance time gps_path weather photo avg_speed) =
         object ["id" .= id,
                 "date" .= date,
                 "distance" .= distance,
                 "time" .= time,
                 "gps_path" .= gps_path,
                 "weather" .= weather,
                 "photo" .= photo,
                 "avg_speed" .= avg_speed]
