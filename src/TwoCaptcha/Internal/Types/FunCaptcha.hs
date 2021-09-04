module TwoCaptcha.Internal.Types.FunCaptcha where

import Control.Lens (Lens', (&), (?~))
import Data.Text (Text)
import TwoCaptcha.Internal.Types.Captcha (Captcha, HasCaptchaLenses (method), HasCommonCaptchaLenses, defaultCaptcha, mkParamLens)

-- | Parameters used to solve FunCaptcha.
newtype FunCaptcha = MkFunCaptcha Captcha deriving (Show)

instance HasCommonCaptchaLenses FunCaptcha

instance HasCaptchaLenses FunCaptcha

-- |
-- Parameters used to solve FunCaptcha.
--
-- Required parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.apiKey'
-- * 'publicKey'
-- * 'pageUrl'
--
-- Optional parameters:
--
-- * 'surl'
-- * 'customData'
-- * 'userAgent'
-- * 'TwoCaptcha.Internal.Types.Captcha.headerACAO'
-- * 'TwoCaptcha.Internal.Types.Captcha.pingback'
-- * 'TwoCaptcha.Internal.Types.Captcha.softId'
-- * 'TwoCaptcha.Internal.Types.Captcha.proxy'
-- * 'TwoCaptcha.Internal.Types.Captcha.proxyType'
funCaptcha :: FunCaptcha
funCaptcha = MkFunCaptcha (defaultCaptcha & method ?~ "funcaptcha")

-- | Value of __pk__ or __data-pkey__ found on the FunCaptcha page.
publicKey :: Lens' FunCaptcha (Maybe Text)
publicKey = mkParamLens "publickey"

-- | Value of __surl__ found on the FunCaptcha page.
surl :: Lens' FunCaptcha (Maybe Text)
surl = mkParamLens "surl"

-- | Full URL of the page where you see the FunCaptcha.
pageUrl :: Lens' FunCaptcha (Maybe Text)
pageUrl = mkParamLens "pageurl"

-- | Custom data to pass to FunCaptcha.
customData :: Text -> Lens' FunCaptcha (Maybe Text)
customData field = mkParamLens ("data[" <> field <> "]")

-- | User agent that will be used by the worker solving the captcha.
userAgent :: Lens' FunCaptcha (Maybe Text)
userAgent = mkParamLens "userAgent"
