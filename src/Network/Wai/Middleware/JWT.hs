{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.JWT
    ( jwt
    )
  where

import           Network.Wai
import           Web.JWT
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Network.HTTP.Types as H
import           Data.Text (Text)
import           Data.Text.IO as TIO
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Maybe (fromJust)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.CaseInsensitive (mk)
import           Data.Word8 (isSpace, toLower)

import           Prelude hiding (exp)

jwt :: Text -> [Text] -> Middleware
jwt sec ignored_routes app req sendRsp =
    let jwtAuth v = do
            maybe
                (sendRsp reject)
                (ifExpired . claims)
                (decodeAndVerifySignature (secret sec) v)

        decodeSubject :: StringOrURI -> Request -> Request
        decodeSubject sub req = do
            req { requestHeaders = ("USER", encodeUtf8 $ stringOrURIToText sub) : requestHeaders req }

        ifExpired :: JWTClaimsSet -> IO ResponseReceived
        ifExpired JWTClaimsSet{..} = do
            curr <- round <$> getPOSIXTime
            if maybe (0) (fromEnum . secondsSinceEpoch) exp <=  curr
                then sendRsp reject
                else app (decodeSubject (fromJust sub) req) sendRsp
    in
      if length (filter (\x -> BS.isPrefixOf (encodeUtf8 x) (rawPathInfo req)) ignored_routes) > 0
          then app req sendRsp
          else maybe
                  (sendRsp reject)
                  (jwtAuth . decodeUtf8)
                  (case lookup "Authorization" headers of
                    Nothing -> mempty
                    Just authHead -> case BS.break isSpace authHead of
                      (strategy, token)
                        | BS.map toLower strategy == "bearer" ->
                          return $ BS.dropWhile isSpace token
                        | otherwise ->
                          mempty)
  where
    headers = requestHeaders req

reject :: Response
reject = do
  responseLBS H.status401 [] mempty
