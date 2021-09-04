module TwoCaptcha.Internal.Types.HCaptcha where

import Control.Lens (Lens', (&), (?~))
import Data.Text (Text)
import TwoCaptcha.Internal.Types.Captcha (Captcha, HasCaptchaLenses (method), HasCommonCaptchaLenses, HasPageURL, HasProxy, defaultCaptcha, mkParamLens, mkParamLensBool)

-- | Parameters used to solve hCaptcha.
newtype HCaptcha = MkHCaptcha Captcha deriving (Show)

instance HasCommonCaptchaLenses HCaptcha

instance HasCaptchaLenses HCaptcha

instance HasPageURL HCaptcha

instance HasProxy HCaptcha

-- |
-- Parameters used to solve hCaptcha.
--
-- Required parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.apiKey'
-- * 'sitekey'
-- * 'TwoCaptcha.Internal.Types.Captcha.pageUrl'
--
-- Optional parameters:
--
-- * 'hInvisible'
-- * 'customData'
-- * 'TwoCaptcha.Internal.Types.Captcha.userAgent'
-- * 'TwoCaptcha.Internal.Types.Captcha.headerACAO'
-- * 'TwoCaptcha.Internal.Types.Captcha.pingback'
-- * 'TwoCaptcha.Internal.Types.Captcha.softId'
-- * 'TwoCaptcha.Internal.Types.Captcha.proxy'
-- * 'TwoCaptcha.Internal.Types.Captcha.proxyType'
hCaptcha :: HCaptcha
hCaptcha = MkHCaptcha (defaultCaptcha & method ?~ "hcaptcha")

-- | Value of __data-sitekey__ parameter on target website.
siteKey :: Lens' HCaptcha (Maybe Text)
siteKey = mkParamLens "sitekey"

-- | Defines if the captcha is invisible. Invisible hCaptchas are currently a rare case.
hInvisible :: Lens' HCaptcha (Maybe Bool)
hInvisible = mkParamLensBool "invisible"

-- |
-- Custom data that is used in some implementations of hCaptcha, mostly with invisible captchas.
-- In most cases, you see it as __rqdata__ inside network requests.
--
-- __IMPORTANT__: you MUST provide 'userAgent' if you submit with 'customData'.
-- The value should match the User-Agent you use when interacting with target website.
customData :: Lens' HCaptcha (Maybe Text)
customData = mkParamLens "data"
