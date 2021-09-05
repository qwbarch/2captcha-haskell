module TwoCaptcha.Internal.Types.TikTokCaptcha where

import Control.Lens (Lens', (&), (?~))
import Data.Text (Text)
import TwoCaptcha.Internal.Types.Captcha (Captcha, HasCaptchaLenses (method), HasCommonCaptchaLenses, HasCookies, HasPageURL, HasProxy, defaultCaptcha, mkParamLens, mkParamLens')

-- | Parameters used to solve a TikTok captcha.
newtype TikTokCaptcha = MkTikTokCaptcha Captcha deriving (Show)

instance HasCommonCaptchaLenses TikTokCaptcha

instance HasCaptchaLenses TikTokCaptcha

instance HasPageURL TikTokCaptcha

instance HasProxy TikTokCaptcha

instance HasCookies TikTokCaptcha

-- |
-- Parameters used to solve a TikTok captcha.
--
-- Required parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.apiKey'
-- * 'TwoCaptcha.Internal.Types.Captcha.cookies'
-- * 'aid'
-- * 'host'
-- * 'TwoCaptcha.Internal.Types.Captcha.pageUrl'
--
-- Optional parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.softId'
-- * 'TwoCaptcha.Internal.Types.Captcha.proxy'
-- * 'TwoCaptcha.Internal.Types.Captcha.proxyType'
tikTokCaptcha :: TikTokCaptcha
tikTokCaptcha = MkTikTokCaptcha (defaultCaptcha & method ?~ "tiktok")

-- | The __aid__ parameter value found on the page.
aid :: Lens' TikTokCaptcha (Maybe Int)
aid = mkParamLens' "aid"

-- | The __host__ parameter value found on the page.
host :: Lens' TikTokCaptcha (Maybe Text)
host = mkParamLens "host"
