module TwoCaptcha.Internal.Client where

import Control.Exception (throw)
import Control.Lens ((&), (.~), (^.), (^?))
import Control.Monad.Catch (Handler (Handler), MonadCatch, MonadThrow (throwM), catches, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (MonadReader (ask))
import Data.Aeson (Value (Null))
import Data.Aeson.Lens (key, _Integer, _Integral, _String)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text (Text, unpack)
import Network.HTTP.Client (HttpException)
import Network.Wreq (JSONError, Options, Response, asJSON, asValue, defaults, param, responseBody)
import Network.Wreq.Session (Session, postWith)
import TwoCaptcha.Internal.Types.Exception (TwoCaptchaException (NetworkException, TwoCaptchaResponseException, UnknownError), readErrorCode)

request :: (MonadIO m, MonadCatch m) => IO (Response ByteString) -> Options -> m Text
request method options = do
  response <- liftIO method
  let statusRequest = do
        body <- response ^? responseBody
        status <- body ^? key "status" . _Integer
        request <- body ^? key "request" . _String
        return (status, request)
  case statusRequest of
    -- 'status' and 'request' fields are missing.
    Nothing -> throwM $ UnknownError "The response is not the expected JSON. This is likely due to 2captcha changing their API."
    Just (status, request) -> do
      -- 'status' 0 means an error was returned.
      if status == 0
        then case readErrorCode (unpack request) of
          -- Parsing the error code failed even though an error was returned. This means the error code is not known yet.
          Left _ -> throwM $ UnknownError ("Invalid error: " <> request)
          -- Parsing the error code succeeded.
          Right errorCode -> throwM $ TwoCaptchaResponseException errorCode
        else -- No error was found.
          pure request
