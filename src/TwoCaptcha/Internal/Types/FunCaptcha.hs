module TwoCaptcha.Internal.Types.FunCaptcha where

import Control.Lens (Lens', (&), (?~))
import Data.Text (Text)
import TwoCaptcha.Internal.Types.Captcha (Captcha, HasCaptchaLenses (method), HasCommonCaptchaLenses, HasPageURL, HasProxy, defaultCaptcha, mkParamLens)

-- | Parameters used to solve FunCaptcha.
newtype FunCaptcha = MkFunCaptcha Captcha deriving (Show)

instance HasCommonCaptchaLenses FunCaptcha

instance HasCaptchaLenses FunCaptcha

instance HasPageURL FunCaptcha

instance HasProxy FunCaptcha

-- |
-- Parameters used to solve FunCaptcha.
--
-- Required parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.apiKey'
-- * 'publicKey'
-- * 'TwoCaptcha.Internal.Types.Captcha.pageUrl'
--
-- Optional parameters:
--
-- * 'surl'
-- * 'customDataField'
-- * 'TwoCaptcha.Internal.Types.Captcha.userAgent'
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

-- | Custom data to pass to FunCaptcha.
customDataField :: Text -> Lens' FunCaptcha (Maybe Text)
customDataField field = mkParamLens ("data[" <> field <> "]")
