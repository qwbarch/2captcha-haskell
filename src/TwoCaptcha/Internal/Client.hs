module TwoCaptcha.Internal.Client where

import Control.Concurrent (threadDelay)
import Control.Lens ((&), (.~), (?~), (^.), (^?))
import Control.Monad.Catch (MonadCatch, MonadThrow (throwM), try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (Value (Null))
import Data.Aeson.Lens (key, _Integer, _String)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text (Text, unpack)
import GHC.Base (Coercible, coerce)
import Network.Wreq (Options, Response, param, responseBody)
import Network.Wreq.Session (Session, getWith, postWith)
import System.Clock (Clock (Monotonic), getTime, toNanoSecs)
import TwoCaptcha.Internal.Types.Captcha (CaptchaId, CaptchaRes (CaptchaRes), HasCaptchaLenses, HasCommonCaptchaLenses (apiKey, headerACAO), PollingInterval, TimeoutDuration, captchaId, defaultCaptchaRes)
import TwoCaptcha.Internal.Types.Exception (TwoCaptchaErrorCode (CaptchaNotReady), TwoCaptchaException (NetworkException, SolvingTimeout, TwoCaptchaResponseException, UnknownError), readErrorCode)

-- | Runs the given http method and adapts errors to 'TwoCaptchaException'.
handle :: (MonadIO m, MonadCatch m) => IO (Response ByteString) -> m Text
handle method =
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
        Nothing -> do
          liftIO $ print response
          throwM $ UnknownError "The response is not the expected JSON. This is likely due to 2captcha changing their API."
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
  submit :: (Coercible Options a, HasCaptchaLenses a, HasCommonCaptchaLenses a) => Session -> a -> m CaptchaId

  -- | Attempt to retrieve the answer of a captcha previously submitted.
  answer :: Session -> CaptchaRes -> m Text

  -- | Submits a captcha and polls for the answer.
  solve :: (Coercible Options a, HasCaptchaLenses a, HasCommonCaptchaLenses a) => Session -> a -> PollingInterval -> TimeoutDuration -> m Text

instance (MonadIO m, MonadCatch m) => TwoCaptchaClient m where
  submit session captcha = handle $ postWith (coerce captcha) session "https://2captcha.com/in.php" Null
  answer session captchaRes = handle $ getWith options session "https://2captcha.com/res.php"
    where
      options = coerce captchaRes & param "action" .~ ["get"] & param "json" .~ ["1"]
  solve session captcha pollingInterval timeoutDuration = do
    captchaId' <- submit session captcha
    let captchaRes =
          defaultCaptchaRes
            & apiKey .~ (captcha ^. apiKey)
            & headerACAO .~ (captcha ^. headerACAO)
            & captchaId ?~ captchaId'
    let time = liftIO $ (\t -> toNanoSecs t `div` 1000000) <$> getTime Monotonic
    startTime <- time
    let pollAnswer previousTime currentTime =
          -- Elapsed time is past the timeout duration
          if currentTime - previousTime >= timeoutDuration
            then throwM SolvingTimeout
            else do
              liftIO $ threadDelay (pollingInterval * 1000)
              -- Attempt to retrieve the answer. If it's not ready yet, retry.
              answerAttempt <- try $ answer session captchaRes
              liftIO $ print answerAttempt
              case answerAttempt of
                Left (TwoCaptchaResponseException CaptchaNotReady) -> do
                  updatedTime <- time
                  pollAnswer currentTime updatedTime
                Left exception -> throwM exception
                Right answer -> pure answer
    pollAnswer startTime startTime
