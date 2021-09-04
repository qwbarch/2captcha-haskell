module TwoCaptcha.Internal.Types.GridCaptcha where

import Control.Lens (Lens')
import Data.Text (Text)
import TwoCaptcha.Internal.Types.Captcha (Captcha, HasCaptchaLenses, HasCommonCaptchaLenses, defaultCaptcha, mkParamLens, mkParamLens', mkParamLensBool, mkPartFileLens, mkPartTextLens)

-- | Parameters used to solve a grid captcha.
newtype GridCaptcha = MkGridCaptcha Captcha deriving (Show)

instance HasCommonCaptchaLenses GridCaptcha

instance HasCaptchaLenses GridCaptcha

-- |
-- Parameters for solving a grid captcha.
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
-- * 'canvas'
-- * 'textInstructions'
-- * 'imgInstructions'
-- * 'reCAPTCHAImage'
-- * 'rows'
-- * 'columns'
-- * 'previousId'
-- * 'cannotAnswer'
-- * 'TwoCaptcha.Internal.Types.Captcha.headerACAO'
-- * 'TwoCaptcha.Internal.Types.Captcha.pingback'
-- * 'TwoCaptcha.Internal.Types.Captcha.softId'
--
-- Possible 'TwoCaptcha.Internal.Types.Captcha.method' values:
--
-- * __post__ - defines that you're sending an image with multipart form
-- * __base64__  - defines that you're sending a base64 encoded image
--
-- Starred required parameter rules:
--
-- * __file__ is only required if __captcha = "post"__
-- * __body__ is only required if __captcha = "base64"__
gridCaptcha :: GridCaptcha
gridCaptcha = MkGridCaptcha defaultCaptcha

-- | Defines that you're sending a reCAPTCHA as an image.
reCAPTCHAImage :: Lens' GridCaptcha (Maybe Bool)
reCAPTCHAImage = mkParamLensBool "recaptcha"

-- | Defines that you want to use a <https://2captcha.com/2captcha-api#canvas canvas> method.
canvas :: Lens' GridCaptcha (Maybe Bool)
canvas = mkParamLensBool "canvas"

-- | File path to captcha image.
file :: Lens' GridCaptcha (Maybe FilePath)
file = mkPartFileLens "file"

-- | Base-64 encoded image.
body :: Lens' GridCaptcha (Maybe Text)
body = mkPartTextLens "body"

-- | Text which is shown to the worker to help solve a captcha.
textInstructions :: Lens' GridCaptcha (Maybe Text)
textInstructions = mkParamLens "textInstructions"

-- | Image file path with instructions on solving a captcha.
imgInstructions :: Lens' GridCaptcha (Maybe FilePath)
imgInstructions = mkPartFileLens "imgInstructions"

-- | Number of rows in reCAPTCHA grid.
rows :: Lens' GridCaptcha (Maybe Int)
rows = mkParamLens' "recaptcharows"

-- | Number of columns in reCAPTCHA grid.
columns :: Lens' GridCaptcha (Maybe Int)
columns = mkParamLens' "recaptchacols"

-- | Id of your previous request with the same captcha challenge.
previousId :: Lens' GridCaptcha (Maybe Text)
previousId = mkParamLens "previousID"

-- | Defines if the captcha can potentially have no images, or possibly cannot be answered.
cannotAnswer :: Lens' GridCaptcha (Maybe Bool)
cannotAnswer = mkParamLensBool "can_no_answer"

-- |
-- The captcha's language:
--
-- 0. Not specified.
-- 1. Cyrillic captcha.
-- 2. Latin captcha.
language :: Lens' GridCaptcha (Maybe Int)
language = mkParamLens' "language"

-- | The captcha's language code. Click <https://2captcha.com/2captcha-api#language here> for a list of supported languages.
languageCode :: Lens' GridCaptcha (Maybe Text)
languageCode = mkParamLens "lang"
