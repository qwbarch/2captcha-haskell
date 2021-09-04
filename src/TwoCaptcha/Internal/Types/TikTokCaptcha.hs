module TwoCaptcha.Internal.Types.TikTokCaptcha where

import Control.Lens (Lens', (&), (?~))
import Data.Text (Text)
import TwoCaptcha.Internal.Types.Captcha (Captcha, HasCaptchaLenses (method), HasCommonCaptchaLenses, defaultCaptcha, mkParamLens, mkParamLens')

-- | Parameters used to solve a TikTok captcha.
newtype TikTokCaptcha = MkTikTokCaptcha Captcha deriving (Show)

instance HasCommonCaptchaLenses TikTokCaptcha

instance HasCaptchaLenses TikTokCaptcha

-- |
-- Parameters used to solve a TikTok captcha.
--
-- Required parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.apiKey'
-- * 'cookies'
-- * 'aid'
-- * 'host'
-- * 'pageUrl'
--
-- Optional parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.headerACAO'
-- * 'TwoCaptcha.Internal.Types.Captcha.pingback'
-- * 'TwoCaptcha.Internal.Types.Captcha.softId'
-- * 'TwoCaptcha.Internal.Types.Captcha.proxy'
-- * 'TwoCaptcha.Internal.Types.Captcha.proxyType'
tikTokCaptcha :: TikTokCaptcha
tikTokCaptcha = MkTikTokCaptcha (defaultCaptcha & method ?~ "tiktok")

-- |
-- Cookies that will be used by the worker solving the TikTok captcha. The used cookies will also be included in the response.
--
-- Format: __KEY1:Value1;KEY2:Value2;__
cookies :: Lens' TikTokCaptcha (Maybe Text)
cookies = mkParamLens "cookies"

-- | The __aid__ parameter value found on the page.
aid :: Lens' TikTokCaptcha (Maybe Int)
aid = mkParamLens' "aid"

-- | The __host__ parameter value found on the page.
host :: Lens' TikTokCaptcha (Maybe Text)
host = mkParamLens "host"

-- | Full URL of the page where the TikTok captcha is found.
pageUrl :: Lens' TikTokCaptcha (Maybe Text)
pageUrl = mkParamLens "pageurl"
