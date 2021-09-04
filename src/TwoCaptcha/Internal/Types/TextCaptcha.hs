module TwoCaptcha.Internal.Types.TextCaptcha where

import Control.Lens (Lens')
import Data.Text (Text)
import TwoCaptcha.Internal.Types.Captcha (Captcha, HasCaptchaLenses, HasCommonCaptchaLenses, HasLanguage, defaultCaptcha, mkParamLens)

-- | Parameters used to solve a text captcha.
newtype TextCaptcha = MkTextCaptcha Captcha deriving (Show)

instance HasCommonCaptchaLenses TextCaptcha

instance HasCaptchaLenses TextCaptcha

instance HasLanguage TextCaptcha

-- |
-- Parameters for solving a text captcha.
--
-- Required parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.apiKey
-- * 'textContent'
--
-- Optional parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.language'
-- * 'TwoCaptcha.Internal.Types.Captcha.languageCode'
-- * 'TwoCaptcha.Internal.Types.Captcha.headerACAO'
-- * 'TwoCaptcha.Internal.Types.Captcha.pingback'
-- * 'TwoCaptcha.Internal.Types.Captcha.softId'
textCaptcha :: TextCaptcha
textCaptcha = MkTextCaptcha defaultCaptcha

-- | The text captcha's content.
textContent :: Lens' TextCaptcha (Maybe Text)
textContent = mkParamLens "textcaptcha"
