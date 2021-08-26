module TwoCaptcha.Internal.Types.ReCaptcha where

import Control.Lens (Lens', iso, (&), (.~))
import Data.Text (Text, pack, unpack)
import GHC.Base (Coercible)
import Network.Wreq (Options)
import Network.Wreq.Lens (param)
import TwoCaptcha.Internal.Types.Captcha (CaptchaLike, defaultCaptcha, lens', lens'')

-- | Default options for a reCAPTCHA.
defaultReCaptcha :: Options
defaultReCaptcha = defaultCaptcha & param "method" .~ ["userrecaptcha"]

-- | Lenses for constructing ReCaptcha options.
class Coercible Options a => ReCaptchaLike a where
  -- | Defines if your ReCaptcha is enterprise.
  enterprise :: Lens' a (Maybe Bool)
  enterprise = lens'' "enterprise"

  -- |
  -- If using 'ReCaptchaV2', this is the value of __k__ or __data-sitekey__ found on the captcha page.
  --
  -- If using 'ReCaptchaV3', this is the value of __sitekey__ found on the captcha page.
  googleKey :: Lens' a (Maybe Text)
  googleKey = lens' "googlekey"

  -- | Full URL of the page where the reCAPTCHA is found.
  pageUrl :: Lens' a (Maybe Text)
  pageUrl = lens' "pageurl"

  -- | Domain used to load the captcha: __google.com__ or __recaptcha.net__.
  domain :: Lens' a (Maybe Text)
  domain = lens' "domain"

  -- |
  -- If 'True', __in.php__ will include the __Access-Control-Allow-Origin:*__ header in the response.
  -- Used for cross-domain AJAX requests in web applications.
  headerACAO :: Lens' a (Maybe Int)
  headerACAO = lens'' "header_acao"

-- | Parameters used to solve reCAPTCHA V2.
newtype ReCaptchaV2 = MkReCaptchaV2 Options deriving (Show)

instance CaptchaLike ReCaptchaV2

instance ReCaptchaLike ReCaptchaV2

-- |
-- Parameters for solving a reCAPTCHA V2.
--
-- Required parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.apiKey'
-- * 'googleKey'
-- * 'pageUrl'
--
-- Optional parameters:
--
-- * 'enterprise'
-- * 'domain'
-- * 'invisible'
-- * 'dataS'
-- * 'cookies'
-- * 'userAgent'
-- * 'headerACAO'
-- * 'TwoCaptcha.Internal.Types.Captcha.pingback'
-- * 'TwoCaptcha.Internal.Types.Captcha.softId'
-- * 'TwoCaptcha.Internal.Types.Captcha.proxy'
-- * 'TwoCaptcha.Internal.Types.Captcha.proxyType'
reCAPTCHAV2 :: ReCaptchaV2
reCAPTCHAV2 = MkReCaptchaV2 $ defaultReCaptcha

-- | Defines if the reCAPTCHA v2 is invisible.
invisible :: Lens' ReCaptchaV2 (Maybe Bool)
invisible = lens' "invisible" . iso (read . unpack <$>) (pack . show <$>)

-- | Value of the __data-s__ parameter found on the reCAPTCHA page. Currently applicable for google services.
dataS :: Lens' ReCaptchaV2 (Maybe Text)
dataS = lens' "data-s"

-- |
-- Cookies that will be used by the worker solving the reCAPTCHA. The used cookies will also be included in the response.
--
-- Format: __KEY1:Value1;KEY2:Value2;__
cookies :: Lens' ReCaptchaV2 (Maybe Text)
cookies = lens' "cookies"

-- | User agent that will be used by the worker solving the reCAPTCHA.
userAgent :: Lens' ReCaptchaV2 (Maybe Text)
userAgent = lens' "userAgent"

-- | Parameters used to solve reCAPTCHA V3.
newtype ReCaptchaV3 = MkReCaptchaV3 Options deriving (Show)

instance CaptchaLike ReCaptchaV3

instance ReCaptchaLike ReCaptchaV3

-- | The score needed for resolution. Currently it's almost impossible to get a token with a score higher than 0.3
minScore :: Lens' ReCaptchaV3 (Maybe Double)
minScore = lens'' "min_score"
