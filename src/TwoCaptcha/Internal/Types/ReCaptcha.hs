module TwoCaptcha.Internal.Types.ReCaptcha where

import Data.Text (Text)
import TwoCaptcha.Internal.Types.Proxy (Proxy)

-- | The possible domains of a reCAPTCHA.
data ReCaptchaDomain = GoogleDomain | ReCaptchaDomain deriving (Show)

-- | Required parameters to solve a reCAPTCHA via the 2captcha API.
data ReCaptcha = ReCaptcha
  { -- | Your 2captcha API <https://2captcha.com/2captcha-api#solving_captchas key>.
    key :: Text,
    -- | Value of __k__ or __data-sitekey__ parameter you found on the captcha page.
    googleKey :: Text,
    -- | Full URL of the page where you see the reCAPTCHA.
    pageUrl :: Text,
    -- | Defines that you're sending reCAPTCHA Enterpise V2.
    enterprise :: Bool,
    -- | Domain used to load the captcha: __google.com__ or __recaptcha.net__.
    domain :: ReCaptchaDomain,
    -- | The visibility of the reCAPTCHA.
    invisible :: Bool,
    -- | Value of the __data-s__ parameter you found on the page. Currently applicable to Google Search and other Google services.
    dataS :: Maybe Text,
    -- | The cookies to send to the worker who solves the captcha. Format: [(key, value)]
    cookies :: [(Text, Text)],
    -- | The user agent to send to the worker who solves the captcha.
    userAgent :: Maybe Text,
    -- |
    -- If enabled, __in.php__ will include Access-Control-Allow-Origin:* header in the response.
    -- Used for cross-domain AJAX requests in web applications. Also supported by __res.php__.
    headerACAO :: Bool,
    -- |
    -- URL for pingback (callback) response that will be sent when captcha is solved.
    -- URL should be registered on the server. Click <https://2captcha.com/2captcha-api#pingback here> for more info.
    pingback :: Text,
    -- | Id of software developer. Developers who integrated their software with 2captcha get 10% of spendings of their users.
    softId :: Int,
    -- | Proxy to send to the worker who solves the captcha.
    proxy :: Proxy
  }
  deriving (Show)
