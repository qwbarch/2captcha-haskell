module TwoCaptcha.Internal.Types.ReCaptcha where

import Control.Lens (Lens', (%~), (&), (.~), (?~))
import Data.Text (Text)
import GHC.Base (Coercible)
import Network.Wreq.Lens (param)
import TwoCaptcha.Internal.Types.Captcha (Captcha, HasCaptchaLenses (method), HasCommonCaptchaLenses, HasPageURL, TimeoutDuration, defaultCaptcha, mkParamLens, mkParamLens', mkParamLensBool, options)

-- | Default parameters for solving a recaptcha. Internal use only.
defaultReCAPTCHA :: Captcha
defaultReCAPTCHA = defaultCaptcha & method ?~ "userrecaptcha"

-- | Lenses for constructing ReCaptcha options.
class Coercible Captcha a => HasReCaptchaLenses a where
  -- | Defines if your ReCaptcha is enterprise.
  enterprise :: Lens' a (Maybe Bool)
  enterprise = mkParamLensBool "enterprise"

  -- |
  -- If using 'ReCaptchaV2', this is the value of __k__ or __data-sitekey__ found on the captcha page.
  --
  -- If using 'ReCaptchaV3', this is the value of __sitekey__ found on the captcha page.
  googleKey :: Lens' a (Maybe Text)
  googleKey = mkParamLens "googlekey"

  -- | Domain used to load the captcha: __google.com__ or __recaptcha.net__.
  domain :: Lens' a (Maybe Text)
  domain = mkParamLens "domain"

-- | Parameters used to solve reCAPTCHA V2.
newtype ReCaptchaV2 = MkReCaptchaV2 Captcha deriving (Show)

instance HasCommonCaptchaLenses ReCaptchaV2

instance HasCaptchaLenses ReCaptchaV2

instance HasReCaptchaLenses ReCaptchaV2

instance HasPageURL ReCaptchaV2

-- |
-- Parameters used to solve reCAPTCHA V2.
--
-- Required parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.apiKey'
-- * 'googleKey'
-- * 'TwoCaptcha.Internal.Types.Captcha.pageUrl'
--
-- Optional parameters:
--
-- * 'enterprise'
-- * 'domain'
-- * 'invisible'
-- * 'dataS'
-- * 'cookies'
-- * 'userAgent'
-- * 'TwoCaptcha.Internal.Types.Captcha.headerACAO'
-- * 'TwoCaptcha.Internal.Types.Captcha.pingback'
-- * 'TwoCaptcha.Internal.Types.Captcha.softId'
-- * 'TwoCaptcha.Internal.Types.Captcha.proxy'
-- * 'TwoCaptcha.Internal.Types.Captcha.proxyType'
reCAPTCHAV2 :: ReCaptchaV2
reCAPTCHAV2 = MkReCaptchaV2 defaultReCAPTCHA

-- | Defines if the reCAPTCHA v2 is invisible.
invisible :: Lens' ReCaptchaV2 (Maybe Bool)
invisible = mkParamLensBool "invisible"

-- | Value of the __data-s__ parameter found on the reCAPTCHA page. Currently applicable for google services.
dataS :: Lens' ReCaptchaV2 (Maybe Text)
dataS = mkParamLens "data-s"

-- |
-- Cookies that will be used by the worker solving the reCAPTCHA. The used cookies will also be included in the response.
--
-- Format: __KEY1:Value1;KEY2:Value2;__
cookies :: Lens' ReCaptchaV2 (Maybe Text)
cookies = mkParamLens "cookies"

-- | User agent that will be used by the worker solving the reCAPTCHA.
userAgent :: Lens' ReCaptchaV2 (Maybe Text)
userAgent = mkParamLens "userAgent"

-- | Parameters used to solve reCAPTCHA V3.
newtype ReCaptchaV3 = MkReCaptchaV3 Captcha deriving (Show)

instance HasCommonCaptchaLenses ReCaptchaV3

instance HasCaptchaLenses ReCaptchaV3

instance HasReCaptchaLenses ReCaptchaV3

instance HasPageURL ReCaptchaV3

-- |
-- Parameters used to solve reCAPTCHA V3.
--
-- Required parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.apiKey'
-- * 'googleKey'
-- * 'TwoCaptcha.Internal.Types.Captcha.pageUrl'
--
-- Optional parameters:
--
-- * 'enterprise'
-- * 'domain'
-- * 'minScore'
-- * 'TwoCaptcha.Internal.Types.Captcha.headerACAO'
-- * 'TwoCaptcha.Internal.Types.Captcha.pingback'
-- * 'TwoCaptcha.Internal.Types.Captcha.softId'
-- * 'TwoCaptcha.Internal.Types.Captcha.proxy'
-- * 'TwoCaptcha.Internal.Types.Captcha.proxyType'
reCAPTCHAV3 :: ReCaptchaV3
reCAPTCHAV3 = MkReCaptchaV3 (defaultReCAPTCHA & options %~ (& param "version" .~ ["v3"]))

-- | The score needed for resolution. Currently it's almost impossible to get a token with a score higher than 0.3
minScore :: Lens' ReCaptchaV3 (Maybe Double)
minScore = mkParamLens' "min_score"

-- | Default reCAPTCHA timeout duration (600 seconds).
reCAPTCHATimeout :: TimeoutDuration
reCAPTCHATimeout = 600000
