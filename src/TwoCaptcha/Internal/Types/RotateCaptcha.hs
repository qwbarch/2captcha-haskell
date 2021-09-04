module TwoCaptcha.Internal.Types.RotateCaptcha where

import Control.Lens (Lens', (&), (?~))
import Data.Text (Text)
import TwoCaptcha.Internal.Types.Captcha (Captcha, HasCaptchaLenses (method), HasCommonCaptchaLenses, defaultCaptcha, mkParamLens', mkPartFileLens, mkPartTextLens)

-- | Parameters used to solve a rotate captcha.
newtype RotateCaptcha = MkRotateCaptcha Captcha deriving (Show)

instance HasCommonCaptchaLenses RotateCaptcha

instance HasCaptchaLenses RotateCaptcha

-- |
-- Parameters used to solve a rotate captcha.
--
-- Required parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.apiKey'
-- * 'TwoCaptcha.Internal.Types.Captcha.method'
-- * 'file'*
-- * 'body'*
--
-- Optional parameters:
--
-- * 'angle'
-- * 'TwoCaptcha.Internal.Types.Captcha.headerACAO'
-- * 'TwoCaptcha.Internal.Types.Captcha.pingback'
-- * 'TwoCaptcha.Internal.Types.Captcha.softId'
--
-- Starred required parameter rules:
--
-- * __file__ is only required if your captcha is sent as a file.
-- * __body__ is only required if your captcha is sent in base64 format.
rotateCaptcha :: RotateCaptcha
rotateCaptcha = MkRotateCaptcha (defaultCaptcha & method ?~ "rotatecaptcha")

-- | Angle for one rotation step in degrees. Defaults to 40 degrees if not specified.
angle :: Lens' RotateCaptcha (Maybe Int)
angle = mkParamLens' "angle"

-- | File path to captcha image.
file :: Lens' RotateCaptcha (Maybe FilePath)
file = mkPartFileLens "file"

-- | Base-64 encoded image.
body :: Lens' RotateCaptcha (Maybe Text)
body = mkPartTextLens "body"
