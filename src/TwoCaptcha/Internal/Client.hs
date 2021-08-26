module TwoCaptcha.Internal.Client where

import Control.Lens ((^?))
import Control.Monad.Catch (MonadCatch, MonadThrow (throwM), try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (MonadReader (ask))
import Data.Aeson (Value (Null))
import Data.Aeson.Lens (key, _Integer, _String)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text (Text, unpack)
import GHC.Base (Coercible, coerce)
import Network.Wreq (Options, Response, responseBody)
import Network.Wreq.Session (Session, getWith, postWith)
import TwoCaptcha.Internal.Types.Exception (TwoCaptchaException (NetworkException, TwoCaptchaResponseException, UnknownError), readErrorCode)

-- | Runs the given http method and adapts errors to 'TwoCaptchaException'.
handle :: (MonadIO m, MonadCatch m) => IO (Response ByteString) -> m Text
handle method = do
  try (liftIO method) >>= \case
    -- HttpException found due to non-200 status code. Rethrow as NetworkException.
    Left exception -> throwM $ NetworkException exception
    Right response -> do
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

-- | Encapsulates the __in.php__ and __res.php__ endpoints for the 2captcha API.
class TwoCaptchaClient m where
  -- | Submit a captcha to be solved by the 2captcha API. Returns a captcha id used for 'answer'.
  submit :: Coercible Options a => a -> m Text

  -- | Attempt to retrieve the answer of a captcha previously submitted. Returns the answer to the captcha.
  answer :: Coercible Options a => a -> m Text

instance (MonadReader Session m, MonadIO m, MonadCatch m) => TwoCaptchaClient m where
  submit captcha = ask >>= \session -> handle $ postWith (coerce captcha) session "https://2captcha.com/in.php" Null
  answer captcha = ask >>= \session -> handle $ getWith (coerce captcha) session "https://2captcha.com/res.php"
