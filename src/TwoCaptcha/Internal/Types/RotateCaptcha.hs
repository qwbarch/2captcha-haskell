module TwoCaptcha.Internal.Types.RotateCaptcha where

import Control.Lens (Lens', (&), (?~))
import TwoCaptcha.Internal.Types.Captcha (Captcha, HasCaptchaLenses (method), HasCommonCaptchaLenses, HasLocalImage, defaultCaptcha, mkParamLens')

-- | Parameters used to solve a rotate captcha.
newtype RotateCaptcha = MkRotateCaptcha Captcha deriving (Show)

instance HasCommonCaptchaLenses RotateCaptcha

instance HasCaptchaLenses RotateCaptcha

instance HasLocalImage RotateCaptcha

-- |
-- Parameters used to solve a rotate captcha.
--
-- Required parameters:
--
-- * 'TwoCaptcha.Internal.Types.Captcha.apiKey'
-- * 'TwoCaptcha.Internal.Types.Captcha.file'*
-- * 'TwoCaptcha.Internal.Types.Captcha.body'*
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
