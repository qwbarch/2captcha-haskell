module TwoCaptcha.Internal.Types.CoordinateCaptcha where

import Control.Lens (Lens')
import Data.Text (Text)
import TwoCaptcha.Internal.Types.Captcha (Captcha, HasCaptchaLenses, HasCommonCaptchaLenses, defaultCaptcha, mkParamLens, mkParamLens', mkParamLensBool, mkPartFileLens, mkPartTextLens)

-- | Parameters used to solve a coordinate captcha.
newtype CoordinateCaptcha = MkCoordinateCaptcha Captcha deriving (Show)

instance HasCommonCaptchaLenses CoordinateCaptcha

instance HasCaptchaLenses CoordinateCaptcha

-- | Parameters used to solve a coordinate captcha.
--
-- Required parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.apiKey'
-- * 'TwoCaptcha.Internal.Types.Captcha.method'
-- * 'reCAPTCHAImage'
-- * 'file'*
-- * 'body'*
--
-- Optional parameters:
--
-- * 'textInstructions'
-- * 'imgInstructions'
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

-- | File path to captcha image.
file :: Lens' CoordinateCaptcha (Maybe FilePath)
file = mkPartFileLens "file"

-- | Base-64 encoded image.
body :: Lens' CoordinateCaptcha (Maybe Text)
body = mkPartTextLens "body"

-- | Text which is shown to the worker to help solve a captcha.
textInstructions :: Lens' CoordinateCaptcha (Maybe Text)
textInstructions = mkParamLens "textInstructions"

-- | Image file path with instructions on solving a captcha.
imgInstructions :: Lens' CoordinateCaptcha (Maybe FilePath)
imgInstructions = mkPartFileLens "imgInstructions"

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
