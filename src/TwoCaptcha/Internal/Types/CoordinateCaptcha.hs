module TwoCaptcha.Internal.Types.CoordinateCaptcha where

import Control.Lens (Lens')
import Data.Text (Text)
import TwoCaptcha.Internal.Types.Captcha (Captcha, HasCaptchaLenses, HasCommonCaptchaLenses, HasLocalImage, defaultCaptcha, mkParamLens, mkParamLens', mkParamLensBool)

-- | Parameters used to solve a coordinate captcha.
newtype CoordinateCaptcha = MkCoordinateCaptcha Captcha deriving (Show)

instance HasCommonCaptchaLenses CoordinateCaptcha

instance HasCaptchaLenses CoordinateCaptcha

instance HasLocalImage CoordinateCaptcha

-- | Parameters used to solve a coordinate captcha.
--
-- Required parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.apiKey'
-- * 'TwoCaptcha.Internal.Types.Captcha.method'
-- * 'reCAPTCHAImage'
-- * 'TwoCaptcha.Internal.Types.Captcha.file'*
-- * 'TwoCaptcha.Internal.Types.Captcha.body'*
--
-- Optional parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.textInstructions'
-- * 'TwoCaptcha.Internal.Types.Captcha.imgInstructions'
-- * 'language'
-- * 'languageCode'
-- * 'TwoCaptcha.Internal.Types.Captcha.headerACAO'
-- * 'TwoCaptcha.Internal.Types.Captcha.pingback'
-- * 'TwoCaptcha.Internal.Types.Captcha.softId'
coordinateCaptcha :: CoordinateCaptcha
coordinateCaptcha = MkCoordinateCaptcha defaultCaptcha

-- | Defines that you're sending a reCAPTCHA as an image.
reCAPTCHAImage :: Lens' CoordinateCaptcha (Maybe Bool)
reCAPTCHAImage = mkParamLensBool "recaptcha"

-- |
-- The captcha's language:
--
-- 0. Not specified.
-- 1. Cyrillic captcha.
-- 2. Latin captcha.
language :: Lens' CoordinateCaptcha (Maybe Int)
language = mkParamLens' "language"

-- | The captcha's language code. Click <https://2captcha.com/2captcha-api#language here> for a list of supported languages.
languageCode :: Lens' CoordinateCaptcha (Maybe Text)
languageCode = mkParamLens "lang"
