module TwoCaptcha.Internal.Types.CapyCaptcha where

import Control.Lens (Lens', (&), (?~))
import Data.Text (Text)
import TwoCaptcha.Internal.Types.Captcha (Captcha, HasCaptchaLenses (method), HasCommonCaptchaLenses, HasPageURL, HasProxy, defaultCaptcha, mkParamLens)

-- | Parameters used to solve a capy puzzle captcha.
newtype CapyCaptcha = MkCapyCaptcha Captcha deriving (Show)

instance HasCommonCaptchaLenses CapyCaptcha

instance HasPageURL CapyCaptcha

instance HasProxy CapyCaptcha

-- |
-- Parameters used to solve a capy puzzle captcha.
--
-- Required parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.apiKey'
-- * 'scriptDomain'
-- * 'TwoCaptcha.Internal.Types.Captcha.pageUrl'
--
-- Optional parameters:
--
-- * 'apiServer'
-- * 'TwoCaptcha.Internal.Types.Captcha.headerACAO'
-- * 'TwoCaptcha.Internal.Types.Captcha.pingback'
-- * 'TwoCaptcha.Internal.Types.Captcha.softId'
-- * 'TwoCaptcha.Internal.Types.Captcha.proxy'
-- * 'TwoCaptcha.Internal.Types.Captcha.proxyType'
capyCaptcha :: CapyCaptcha
capyCaptcha = MkCapyCaptcha (defaultCaptcha & method ?~ "capy")

-- | Value of __captchakey__ parameter you found on page.
captchaKey :: Lens' CapyCaptcha (Maybe Text)
captchaKey = mkParamLens "captchakey"

-- |
-- The domain part of the script URL found on page.
--
-- If not specified, defaults to: https://jp.api.capy.me/
scriptDomain :: Lens' CapyCaptcha (Maybe Text)
scriptDomain = mkParamLens "api_server"
