module TwoCaptcha.Internal.Types.TextCaptcha where

import Control.Lens (Lens')
import Data.Text (Text)
import TwoCaptcha.Internal.Types.Captcha (Captcha, HasCaptchaLenses, HasCommonCaptchaLenses, defaultCaptcha, mkParamLens, mkParamLens')

-- | Parameters used to solve a text captcha.
newtype TextCaptcha = MkTextCaptcha Captcha deriving (Show)

instance HasCommonCaptchaLenses TextCaptcha

instance HasCaptchaLenses TextCaptcha

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
-- * 'language'
-- * 'languageCode'
-- * 'TwoCaptcha.Internal.Types.Captcha.headerACAO'
-- * 'TwoCaptcha.Internal.Types.Captcha.pingback'
-- * 'TwoCaptcha.Internal.Types.Captcha.softId'
textCaptcha :: TextCaptcha
textCaptcha = MkTextCaptcha defaultCaptcha

-- |
-- The captcha's language:
--
-- 0. Not specified.
-- 1. Cyrillic captcha.
-- 2. Latin captcha.
language :: Lens' TextCaptcha (Maybe Int)
language = mkParamLens' "language"

-- | The captcha's language code. Click <https://2captcha.com/2captcha-api#language here> for a list of supported languages.
languageCode :: Lens' TextCaptcha (Maybe Text)
languageCode = mkParamLens "lang"

-- | The text captcha's content.
textContent :: Lens' TextCaptcha (Maybe Text)
textContent = mkParamLens "textcaptcha"
