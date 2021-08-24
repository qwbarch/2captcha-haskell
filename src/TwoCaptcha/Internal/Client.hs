module TwoCaptcha.Internal.Client where

import Control.Lens ((&), (.~), (^?))
import Control.Monad.Catch (MonadCatch, MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (MonadReader (ask))
import Data.Aeson (Value (Null))
import Data.Aeson.Lens (_Integer, _String)
import qualified Data.Aeson.Lens as Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text (Text, empty, pack, unpack)
import Network.Wreq (Options, Response, defaults, param, responseBody)
import Network.Wreq.Session (Session, getWith, postWith)
import TwoCaptcha.Internal.Types.Exception (TwoCaptchaException (TwoCaptchaResponseException, UnknownError), readErrorCode)
import TwoCaptcha.Internal.Types.Proxy (Proxy (address, auth, port, proxyType), ProxyAuth (password, user))
import TwoCaptcha.Internal.Types.ReCaptcha (ReCaptcha (cookies, dataS, domain, enterprise, googleKey, headerACAO, invisible, key, pingback, proxy, softId, userAgent), domainUrl)

-- | Handles the given http request and rethrows exceptions.
handle :: (MonadIO m, MonadCatch m) => IO (Response ByteString) -> m Text
handle method = do
  response <- liftIO method
  let statusRequest = do
        body <- response ^? responseBody
        status <- body ^? Aeson.key "status" . _Integer
        request <- body ^? Aeson.key "request" . _String
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

-- | Default options to be used in requests.
defaultOpts :: Options
defaultOpts = defaults & param "json" .~ ["1"] -- Enable JSON responses.

withKeyOpts :: Text -> Options -> Options
withKeyOpts key opts = opts & param "key" .~ [key]

withMethodOpts :: Text -> Options -> Options
withMethodOpts method opts = opts & param "method" .~ [method]

withEnterpriseOpts :: Bool -> Options -> Options
withEnterpriseOpts enterprise opts =
  if enterprise
    then opts & param "enterprise" .~ ["1"]
    else opts

withGoogleKeyOpts :: Text -> Options -> Options
withGoogleKeyOpts googleKey opts = opts & param "googlekey" .~ [googleKey]

withPageUrlOpts :: Text -> Options -> Options
withPageUrlOpts pageUrl opts = opts & param "pageurl" .~ [pageUrl]

withDomainOpts :: Text -> Options -> Options
withDomainOpts domain opts = opts & param "domain" .~ [domain]

withInvisibleOpts :: Bool -> Options -> Options
withInvisibleOpts invisible opts =
  if not invisible
    then opts & param "invisible" .~ ["1"]
    else opts

withDataSOpts :: Maybe Text -> Options -> Options
withDataSOpts (Just dataS) opts = opts & param "data-s" .~ [dataS]
withDataSOpts Nothing opts = opts

withCookieOpts :: [(Text, Text)] -> Options -> Options
withCookieOpts cookies opts =
  if null cookies
    then opts
    else opts & param "cookies" .~ [foldr (\(key, value) acc -> key <> ":" <> value <> ";" <> acc) empty cookies]

withUserAgentOpts :: Maybe Text -> Options -> Options
withUserAgentOpts (Just userAgent) opts = opts & param "userAgent" .~ [userAgent]
withUserAgentOpts Nothing opts = opts

withHeaderACAOOpts :: Bool -> Options -> Options
withHeaderACAOOpts headerACAO opts =
  if headerACAO
    then opts & param "header_acao" .~ ["1"]
    else opts

withPingbackOpts :: Maybe Text -> Options -> Options
withPingbackOpts (Just pingback) opts = opts & param "pingback" .~ [pingback]
withPingbackOpts Nothing opts = opts

withSoftIdOpts :: Maybe Int -> Options -> Options
withSoftIdOpts (Just softId) opts = opts & param "soft_id" .~ [pack $ show softId]
withSoftIdOpts Nothing opts = opts

withProxyOpts :: Maybe Proxy -> Options -> Options
withProxyOpts (Just proxy) opts =
  opts
    & param "proxy" .~ [proxyAuth <> proxyInfo]
    & param "proxytype" .~ [pack . show $ proxyType proxy]
  where
    proxyAuth = case auth proxy of
      Just authentication -> user authentication <> ":" <> password authentication <> "@"
      Nothing -> empty
    proxyInfo = address proxy <> pack (show (port proxy))
withProxyOpts Nothing opts = opts

class TwoCaptchaClient m where
  -- | Submits a captcha to 2captcha's API.
  submit :: Options -> m Text

  -- | Retrieves the captcha solution from 2captcha's API.
  response :: Options -> m Text

  -- | Submits a reCAPTCHA to be solved by the 2captcha API.
  submitReCaptcha :: ReCaptcha -> m Text
  submitReCaptcha payload = submit opts
    where
      opts =
        withKeyOpts (key payload)
          . withMethodOpts "userrecaptcha"
          . withEnterpriseOpts (enterprise payload)
          . withGoogleKeyOpts (googleKey payload)
          . withDomainOpts (domainUrl $ domain payload)
          . withInvisibleOpts (invisible payload)
          . withDataSOpts (dataS payload)
          . withCookieOpts (cookies payload)
          . withUserAgentOpts (userAgent payload)
          . withHeaderACAOOpts (headerACAO payload)
          . withPingbackOpts (pingback payload)
          . withSoftIdOpts (softId payload)
          . withProxyOpts (proxy payload)
          $ defaultOpts

instance (MonadReader Session m, MonadIO m, MonadCatch m) => TwoCaptchaClient m where
  submit options = ask >>= \session -> handle $ postWith options session "https://2captcha.com/in.php" Null
  response options = ask >>= \session -> handle $ getWith options session "https://2captcha.com/res.php"
