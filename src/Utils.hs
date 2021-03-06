{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Utils ( addToken ) where

import Domain

import           Data.Aeson
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           Data.Text (Text, pack)
import           Data.Text.Lazy (fromStrict)
import           Data.Time
import           Data.Time.Clock.POSIX
import           Prelude hiding (id, exp)
import           Web.JWT


calculateExpiration :: UTCTime -> NominalDiffTime
calculateExpiration time =
  let (year, month, dayOfMonth) = toGregorian $ utctDay time
      current_day = fromGregorian year month dayOfMonth
      new_day = addDays 90 current_day
  in utcTimeToPOSIXSeconds (UTCTime new_day (fromIntegral 0))

addToken :: UTCTime -> User -> Maybe User
addToken currentTime user =
  let
      cs = def { -- def returns a default JWTClaimsSet
         sub = stringOrURI (pack (show (id (user :: User))))
         , exp = numericDate $ calculateExpiration currentTime
         , unregisteredClaims = Map.fromList [("http://example.com/is_root", (Bool True))]
      }
      key = secret "1978@rpa"
      token = encodeSigned HS256 key cs
  in Just user { access_token = fromStrict token }
