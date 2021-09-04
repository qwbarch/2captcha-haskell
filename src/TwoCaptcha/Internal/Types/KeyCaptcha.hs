module TwoCaptcha.Internal.Types.KeyCaptcha where

import Control.Lens (Lens', (&), (?~))
import Data.Text (Text)
import TwoCaptcha.Internal.Types.Captcha (Captcha, HasCaptchaLenses (method), HasCommonCaptchaLenses, HasPageURL, defaultCaptcha, mkParamLens)

-- | Parameters used to solve KeyCaptcha.
newtype KeyCaptcha = MkKeyCaptcha Captcha deriving (Show)

instance HasCommonCaptchaLenses KeyCaptcha

instance HasCaptchaLenses KeyCaptcha

instance HasPageURL KeyCaptcha

-- |
-- Parameters used to solve a KeyCaptcha.
--
-- Required parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.apiKey'
-- * 'userId'
-- * 'sessionId'
-- * 'webServerSign'
-- * 'webServerSign2'
-- * 'TwoCaptchal.Internal.Types.Captcha.pageUrl'
--
-- Optional parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.headerACAO'
-- * 'TwoCaptcha.Internal.Types.Captcha.pingback'
-- * 'TwoCaptcha.Internal.Types.Captcha.softId'
keyCaptcha :: KeyCaptcha
keyCaptcha = MkKeyCaptcha (defaultCaptcha & method ?~ "keycaptcha")

-- | Value of __s_s_c_user_id__ parameter you found on page.
userId :: Lens' KeyCaptcha (Maybe Text)
userId = mkParamLens "s_s_c_user_id"

-- | Value of __s_s_c_session_id__ parameter you found on page.
sessionId :: Lens' KeyCaptcha (Maybe Text)
sessionId = mkParamLens "s_s_c_session_id"

-- | Value of __s_s_c_web_server_sign__ parameter you found on page.
webServerSign :: Lens' KeyCaptcha (Maybe Text)
webServerSign = mkParamLens "s_s_c_web_server_sign"

-- | Value of __s_s_c_web_server_sign2__ parameter you found on page.
webServerSign2 :: Lens' KeyCaptcha (Maybe Text)
webServerSign2 = mkParamLens "s_s_c_web_server_sign2"
