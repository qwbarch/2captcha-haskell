module TwoCaptcha.Internal.Types.CoordinateCaptcha where

import Control.Lens (Lens')
import TwoCaptcha.Internal.Types.Captcha (Captcha, HasCaptchaLenses, HasCommonCaptchaLenses, HasLanguage, HasLocalImage, defaultCaptcha, mkParamLensBool)

-- | Parameters used to solve a coordinate captcha.
newtype CoordinateCaptcha = MkCoordinateCaptcha Captcha deriving (Show)

instance HasCommonCaptchaLenses CoordinateCaptcha

instance HasCaptchaLenses CoordinateCaptcha

instance HasLocalImage CoordinateCaptcha

instance HasLanguage CoordinateCaptcha

-- | Parameters used to solve a coordinate captcha.
--
-- Required parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.apiKey'
-- * 'TwoCaptcha.Internal.Types.Captcha.method'
-- * 'coordinatesCaptcha'
-- * 'TwoCaptcha.Internal.Types.Captcha.file'*
-- * 'TwoCaptcha.Internal.Types.Captcha.body'*
--
-- Optional parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.textInstructions'
-- * 'TwoCaptcha.Internal.Types.Captcha.imgInstructions'
-- * 'TwoCaptcha.Internal.Types.Captcha.language'
-- * 'TwoCaptcha.Internal.Types.Captcha.languageCode'
-- * 'TwoCaptcha.Internal.Types.Captcha.headerACAO'
-- * 'TwoCaptcha.Internal.Types.Captcha.pingback'
-- * 'TwoCaptcha.Internal.Types.Captcha.softId'
coordinateCaptcha :: CoordinateCaptcha
coordinateCaptcha = MkCoordinateCaptcha defaultCaptcha

-- | Defines that you're sending a reCAPTCHA as an image.
coordinatesCaptcha :: Lens' CoordinateCaptcha (Maybe Bool)
coordinatesCaptcha = mkParamLensBool "coordinatescaptcha"
