module TwoCaptcha.Internal.Types.GeeTestCaptcha where

import Control.Lens (Lens', (&), (?~))
import Data.Text (Text)
import TwoCaptcha.Internal.Types.Captcha (Captcha, HasCaptchaLenses (method), HasCommonCaptchaLenses, HasPageURL, HasProxy, defaultCaptcha, mkParamLens)

-- | Parameters used to solve a GeeTest captcha.
newtype GeeTestCaptcha = MkGeeTestCaptcha Captcha deriving (Show)

instance HasCommonCaptchaLenses GeeTestCaptcha

instance HasCaptchaLenses GeeTestCaptcha

instance HasPageURL GeeTestCaptcha

instance HasProxy GeeTestCaptcha

-- |
-- Parameters used to solve a GeeTest captcha.
--
-- Required parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.apiKey'
-- * 'gt'
-- * 'challenge'
-- * 'TwoCaptcha.Internal.Types.Captcha.pageUrl'
--
-- Optional parameters:
--
-- * 'apiServer'
-- * 'TwoCaptcha.Internal.Types.Captcha.userAgent'
-- * 'TwoCaptcha.Internal.Types.Captcha.headerACAO'
-- * 'TwoCaptcha.Internal.Types.Captcha.pingback'
-- * 'TwoCaptcha.Internal.Types.Captcha.softId'
-- * 'TwoCaptcha.Internal.Types.Captcha.proxy'
-- * 'TwoCaptcha.Internal.Types.Captcha.proxyType'
geeTestCaptcha :: GeeTestCaptcha
geeTestCaptcha = MkGeeTestCaptcha (defaultCaptcha & method ?~ "geetest")

-- | Value of __gt__ parameter you found on target website.
gt :: Lens' GeeTestCaptcha (Maybe Text)
gt = mkParamLens "gt"

-- | Value of __challenge__ parameter you found on target website.
challenge :: Lens' GeeTestCaptcha (Maybe Text)
challenge = mkParamLens "challenge"

-- | Value of __api_server__ parameter you found on target website.
apiServer :: Lens' GeeTestCaptcha (Maybe Text)
apiServer = mkParamLens "api_server"
