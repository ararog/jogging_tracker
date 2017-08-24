
module Network.Wai.Middleware.User
    ( load_user
    )
  where

import           Network.Wai
import qualified Network.HTTP.Types as H
import           Data.Text (Text)
import           Data.Text.IO as TIO
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.CaseInsensitive (mk)
import           Data.Word8 (isSpace, toLower)

load_user :: Pool Connection -> Middleware
load_user pool app req sendRsp = do
    headers <- requestHeaders req
    maybeUser <- liftIO $ findUser pool headers["USER"]
    app req sendRsp
